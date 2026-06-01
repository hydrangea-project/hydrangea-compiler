"use strict";

const fs = require("node:fs/promises");
const path = require("node:path");
const vscode = require("vscode");
const { Language, Parser, Query } = require("web-tree-sitter");

const legend = new vscode.SemanticTokensLegend(
  [
    "comment",
    "keyword",
    "operator",
    "string",
    "number",
    "function",
    "property",
    "parameter",
    "type"
  ],
  ["defaultLibrary"]
);

const captureToToken = new Map([
  ["comment", { type: "comment" }],
  ["keyword", { type: "keyword" }],
  ["boolean", { type: "keyword" }],
  ["operator", { type: "operator" }],
  ["string", { type: "string" }],
  ["number", { type: "number" }],
  ["number.float", { type: "number" }],
  ["function", { type: "function" }],
  ["property", { type: "property" }],
  ["variable.parameter", { type: "parameter" }],
  ["type.parameter", { type: "type" }],
  ["type.builtin", { type: "type", modifiers: ["defaultLibrary"] }]
]);

const capturePriority = new Map([
  ["comment", 100],
  ["string", 90],
  ["function", 80],
  ["property", 80],
  ["variable.parameter", 80],
  ["type.parameter", 80],
  ["type.builtin", 80],
  ["keyword", 70],
  ["boolean", 70],
  ["operator", 70],
  ["number", 70],
  ["number.float", 70]
]);

class HydrangeaSemanticTokensProvider {
  constructor(context) {
    this.context = context;
    this.readyPromise = undefined;
    this.changeEmitter = new vscode.EventEmitter();
    this.onDidChangeSemanticTokens = this.changeEmitter.event;
  }

  async provideDocumentSemanticTokens(document) {
    await this.ensureReady();

    const tree = this.parser.parse(document.getText());
    const captures = this.query.captures(tree.rootNode);
    const tokens = collectTokens(document, captures);
    const builder = new vscode.SemanticTokensBuilder(legend);

    for (const token of tokens) {
      builder.push(
        token.line,
        token.startCharacter,
        token.length,
        token.type,
        token.modifiers
      );
    }

    return builder.build();
  }

  async ensureReady() {
    this.readyPromise ??= this.initialize();
    return this.readyPromise;
  }

  async initialize() {
    await Parser.init({
      locateFile: () =>
        this.context.asAbsolutePath(
          path.join("node_modules", "web-tree-sitter", "tree-sitter.wasm")
        )
    });

    const language = await Language.load(
      this.context.asAbsolutePath(
        path.join("assets", "tree-sitter-hydrangea.wasm")
      )
    );

    this.parser = new Parser();
    this.parser.setLanguage(language);

    const highlightsSource = await fs.readFile(
      this.context.asAbsolutePath(path.join("queries", "highlights.scm")),
      "utf8"
    );
    this.query = new Query(language, highlightsSource);
  }

  invalidate(document) {
    if (document.languageId === "hydrangea") {
      this.changeEmitter.fire();
    }
  }

  dispose() {
    this.changeEmitter.dispose();
  }
}

function collectTokens(document, captures) {
  const selected = new Map();

  for (const capture of captures) {
    const tokenInfo = captureToToken.get(capture.name);
    if (!tokenInfo) {
      continue;
    }

    for (const segment of splitCapture(document, capture, tokenInfo)) {
      const key = `${segment.line}:${segment.startCharacter}:${segment.length}`;
      const current = selected.get(key);
      if (!current || segment.priority > current.priority) {
        selected.set(key, segment);
      }
    }
  }

  return Array.from(selected.values())
    .sort(
      (left, right) =>
        left.line - right.line ||
        left.startCharacter - right.startCharacter ||
        left.length - right.length
    )
    .filter((token, index, items) => {
      if (index === 0) {
        return true;
      }

      const previous = items[index - 1];
      if (previous.line !== token.line) {
        return true;
      }

      return previous.startCharacter + previous.length <= token.startCharacter;
    });
}

function splitCapture(document, capture, tokenInfo) {
  const segments = [];
  const start = capture.node.startPosition;
  const end = capture.node.endPosition;

  for (let line = start.row; line <= end.row; line += 1) {
    const lineLength = document.lineAt(line).text.length;
    const startCharacter = line === start.row ? start.column : 0;
    const endCharacter = line === end.row ? end.column : lineLength;
    const length = endCharacter - startCharacter;

    if (length <= 0) {
      continue;
    }

    segments.push({
      line,
      startCharacter,
      length,
      type: tokenInfo.type,
      modifiers: tokenInfo.modifiers ?? [],
      priority: capturePriority.get(capture.name) ?? 0
    });
  }

  return segments;
}

function activate(context) {
  const provider = new HydrangeaSemanticTokensProvider(context);

  context.subscriptions.push(
    provider,
    vscode.languages.registerDocumentSemanticTokensProvider(
      { language: "hydrangea" },
      provider,
      legend
    ),
    vscode.workspace.onDidChangeTextDocument((event) =>
      provider.invalidate(event.document)
    ),
    vscode.workspace.onDidOpenTextDocument((document) =>
      provider.invalidate(document)
    )
  );
}

module.exports = {
  activate
};
