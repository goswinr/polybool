//
// polybool - Boolean operations on polygons (union, intersection, etc)
// by Sean Connelly (@velipso), https://sean.fun
// Project Home: https://github.com/velipso/polybool
// SPDX-License-Identifier: 0BSD
//

import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  GeometryEpsilon,
  PolyBool,
  type Polygon as PolyBoolPolygon,
} from './polybool';

type OperationName = 'union' | 'intersection' | 'difference' | 'xor';
type Position = [number, number];
type Ring = Position[];
type PolygonCoordinates = Ring[];
type MultiPolygonCoordinates = PolygonCoordinates[];

interface GeoJsonFeatureCollection {
  type: 'FeatureCollection';
  features: GeoJsonFeature[];
}

interface GeoJsonFeature {
  type: 'Feature';
  properties?: {
    options?: {
      precision?: number;
    };
  };
  geometry: GeoJsonGeometry;
}

type GeoJsonGeometry =
  | {
      type: 'Polygon';
      coordinates: PolygonCoordinates;
    }
  | {
      type: 'MultiPolygon';
      coordinates: MultiPolygonCoordinates;
    };

interface TestFailure {
  target: string;
  operation: OperationName;
  reason: string;
}

interface ComparisonResult {
  equal: boolean;
  reason?: string;
}

interface NormalizedRing {
  fingerprint: string;
  points: Ring;
}

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const endToEndDir = path.resolve(
  __dirname,
  '..',
  '..',
  'polyclip-ts',
  'test',
  'end-to-end',
);
const showFailures = process.argv.includes('--show-failures');
const operations: OperationName[] = [
  'union',
  'intersection',
  'difference',
  'xor',
];
const defaultTolerance = 1e-9;

let total = 0;
let pass = 0;
let fail = 0;
const failures: TestFailure[] = [];
const stats = new Map<OperationName, { pass: number; fail: number }>();
for (const operation of operations) {
  stats.set(operation, { pass: 0, fail: 0 });
}

for (const target of fs.readdirSync(endToEndDir)) {
  if (target.startsWith('.')) {
    continue;
  }

  const targetDir = path.join(endToEndDir, target);
  if (!fs.statSync(targetDir).isDirectory()) {
    continue;
  }

  const argsGeoJson = readJsonFile<GeoJsonFeatureCollection>(
    path.join(targetDir, 'args.geojson'),
  );
  const args = argsGeoJson.features.map((feature) =>
    geoJsonGeometryToPolyBoolPolygon(feature.geometry),
  );

  const operationFiles = fs
    .readdirSync(targetDir)
    .filter((fileName) => fileName.endsWith('.geojson') && fileName !== 'args.geojson')
    .flatMap((fileName) => expandOperationFile(fileName).map((operation) => ({
      operation,
      resultPath: path.join(targetDir, fileName),
    })));

  for (const { operation, resultPath } of operationFiles) {
    total++;
    const stat = stats.get(operation);

    try {
      const resultGeoJson = readJsonFile<GeoJsonFeature>(resultPath);
      const tolerance =
        resultGeoJson.properties?.options?.precision ?? defaultTolerance;
      const polybool = new PolyBool(new GeometryEpsilon(tolerance));
      const actual = runOperation(polybool, operation, args);
      const expected = geoJsonGeometryToPolyBoolPolygon(resultGeoJson.geometry);
      const comparison = comparePolygons(actual, expected, tolerance);

      if (!comparison.equal) {
        throw new Error(comparison.reason ?? 'Result mismatch');
      }

      pass++;
      if (stat) {
        stat.pass++;
      }
    } catch (error) {
      fail++;
      if (stat) {
        stat.fail++;
      }
      failures.push({
        target,
        operation,
        reason: error instanceof Error ? error.message : String(error),
      });
    }
  }
}

console.log('PolyBool vs polyclip-ts end-to-end fixtures');
console.log(`Total: ${total}`);
console.log(`Pass: ${pass}`);
console.log(`Fail: ${fail}`);
for (const operation of operations) {
  const stat = stats.get(operation);
  if (!stat) {
    continue;
  }
  console.log(`${operation}: ${stat.pass} passed, ${stat.fail} failed`);
}

if (showFailures && failures.length > 0) {
  console.log('\nFailures:');
  for (const failure of failures) {
    console.log(`${failure.target}/${failure.operation}: ${failure.reason}`);
  }
}

function readJsonFile<T>(filePath: string): T {
  return JSON.parse(fs.readFileSync(filePath, 'utf8')) as T;
}

function expandOperationFile(fileName: string): OperationName[] {
  const operationName = path.basename(fileName, '.geojson');
  if (operationName === 'all') {
    return [...operations];
  }
  if (!operations.includes(operationName as OperationName)) {
    throw new Error(`Unsupported operation file '${fileName}'`);
  }
  return [operationName as OperationName];
}

function geoJsonGeometryToPolyBoolPolygon(
  geometry: GeoJsonGeometry,
): PolyBoolPolygon {
  const regions =
    geometry.type === 'Polygon'
      ? geometry.coordinates.map(stripClosingPoint)
      : geometry.coordinates.flatMap((polygon) => polygon.map(stripClosingPoint));
  return {
    regions: regions.filter((region) => region.length > 0),
    inverted: false,
  };
}

function stripClosingPoint(ring: Ring): Ring {
  if (ring.length === 0) {
    return [];
  }
  if (pointsExactlyEqual(ring[0], ring[ring.length - 1])) {
    return ring.slice(0, -1);
  }
  return ring.slice();
}

function pointsExactlyEqual(a: Position, b: Position): boolean {
  return a[0] === b[0] && a[1] === b[1];
}

function runOperation(
  polybool: PolyBool,
  operation: OperationName,
  polygons: PolyBoolPolygon[],
): PolyBoolPolygon {
  if (polygons.length === 0) {
    return { regions: [], inverted: false };
  }
  if (polygons.length === 1) {
    return polybool.polygon(polybool.segments(polygons[0]));
  }

  let result = polygons[0];
  for (let index = 1; index < polygons.length; index++) {
    const polygon = polygons[index];
    switch (operation) {
      case 'union':
        result = polybool.union(result, polygon);
        break;
      case 'intersection':
        result = polybool.intersect(result, polygon);
        break;
      case 'difference':
        result = polybool.difference(result, polygon);
        break;
      case 'xor':
        result = polybool.xor(result, polygon);
        break;
    }
  }

  return result;
}

function comparePolygons(
  actual: PolyBoolPolygon,
  expected: PolyBoolPolygon,
  tolerance: number,
): ComparisonResult {
  if (actual.inverted !== expected.inverted) {
    return {
      equal: false,
      reason: `Inverted flag mismatch: expected ${expected.inverted}, got ${actual.inverted}`,
    };
  }

  const normalizedActual = normalizeRegions(actual.regions, tolerance);
  const normalizedExpected = normalizeRegions(expected.regions, tolerance);

  if (normalizedActual.length !== normalizedExpected.length) {
    return {
      equal: false,
      reason: `Ring count mismatch: expected ${normalizedExpected.length}, got ${normalizedActual.length}`,
    };
  }

  for (let ringIndex = 0; ringIndex < normalizedActual.length; ringIndex++) {
    const actualRing = normalizedActual[ringIndex];
    const expectedRing = normalizedExpected[ringIndex];
    if (actualRing.points.length !== expectedRing.points.length) {
      return {
        equal: false,
        reason: `Point count mismatch in ring ${ringIndex}: expected ${expectedRing.points.length}, got ${actualRing.points.length}`,
      };
    }
    for (let pointIndex = 0; pointIndex < actualRing.points.length; pointIndex++) {
      const actualPoint = actualRing.points[pointIndex];
      const expectedPoint = expectedRing.points[pointIndex];
      if (
        !numbersWithinTolerance(actualPoint[0], expectedPoint[0], tolerance) ||
        !numbersWithinTolerance(actualPoint[1], expectedPoint[1], tolerance)
      ) {
        return {
          equal: false,
          reason: `Coordinate mismatch in ring ${ringIndex}, point ${pointIndex}: expected [${expectedPoint[0]}, ${expectedPoint[1]}], got [${actualPoint[0]}, ${actualPoint[1]}]`,
        };
      }
    }
  }

  return { equal: true };
}

function normalizeRegions(
  regions: PolyBoolPolygon['regions'],
  tolerance: number,
): NormalizedRing[] {
  return regions
    .map((region) => normalizeRing(region, tolerance))
    .sort((a, b) => compareFingerprints(a.fingerprint, b.fingerprint));
}

function normalizeRing(
  region: PolyBoolPolygon['regions'][number],
  tolerance: number,
): NormalizedRing {
  const ring = region.map((point) => {
    if (point.length !== 2) {
      throw new Error('Bezier segments are not supported by the GeoJSON runner');
    }
    return roundPoint(point, tolerance);
  });

  if (ring.length <= 1) {
    const fingerprint = JSON.stringify(ring);
    return { fingerprint, points: ring };
  }

  let best = rotateRing(ring, 0);
  let bestFingerprint = ringFingerprint(best);
  for (const candidateRing of [ring, [...ring].reverse()]) {
    for (let index = 0; index < candidateRing.length; index++) {
      const candidate = rotateRing(candidateRing, index);
      const candidateFingerprint = ringFingerprint(candidate);
      if (compareFingerprints(candidateFingerprint, bestFingerprint) < 0) {
        best = candidate;
        bestFingerprint = candidateFingerprint;
      }
    }
  }

  return { fingerprint: bestFingerprint, points: best };
}

function roundPoint(point: Position, tolerance: number): Position {
  return [roundNumber(point[0], tolerance), roundNumber(point[1], tolerance)];
}

function roundNumber(value: number, tolerance: number): number {
  const digits = precisionDigits(tolerance);
  const rounded = Number(value.toFixed(digits));
  return Object.is(rounded, -0) ? 0 : rounded;
}

function precisionDigits(tolerance: number): number {
  if (!Number.isFinite(tolerance) || tolerance <= 0) {
    return 12;
  }
  const digits = Math.ceil(-Math.log10(tolerance));
  return Math.min(15, Math.max(0, digits));
}

function rotateRing(ring: Ring, startIndex: number): Ring {
  return ring.slice(startIndex).concat(ring.slice(0, startIndex));
}

function ringFingerprint(ring: Ring): string {
  return JSON.stringify(ring);
}

function compareFingerprints(a: string, b: string): number {
  if (a === b) {
    return 0;
  }
  return a < b ? -1 : 1;
}

function numbersWithinTolerance(
  actual: number,
  expected: number,
  tolerance: number,
): boolean {
  return Math.abs(actual - expected) <= tolerance;
}