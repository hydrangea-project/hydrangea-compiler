import { cp, mkdir } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const scriptDir = path.dirname(fileURLToPath(import.meta.url));
const extensionDir = path.resolve(scriptDir, "..");
const repoDir = path.resolve(extensionDir, "..");

const grammarWasmPath = path.join(
  repoDir,
  "tree-sitter-hydrangea",
  "dist",
  "tree-sitter-hydrangea.wasm"
);
const highlightsPath = path.join(
  repoDir,
  "tree-sitter-hydrangea",
  "queries",
  "highlights.scm"
);

const extensionWasmPath = path.join(
  extensionDir,
  "assets",
  "tree-sitter-hydrangea.wasm"
);
const extensionHighlightsPath = path.join(
  extensionDir,
  "queries",
  "highlights.scm"
);

await mkdir(path.dirname(extensionWasmPath), { recursive: true });
await mkdir(path.dirname(extensionHighlightsPath), { recursive: true });

await cp(grammarWasmPath, extensionWasmPath);
await cp(highlightsPath, extensionHighlightsPath);
