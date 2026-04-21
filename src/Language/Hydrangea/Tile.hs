{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.Tile
--
-- Backwards-compatible statement-list tiling entrypoint backed by the
-- polyhedral identity-order tiler.
module Language.Hydrangea.Tile
  ( tileStmts2
  ) where

import Language.Hydrangea.CFG
import Language.Hydrangea.Polyhedral (polyhedralIdentityTileProgram2)

tileStmts2 :: [Stmt] -> [Stmt]
tileStmts2 stmts =
  case polyhedralIdentityTileProgram2 (Program [mkProc "tile_stmts2" [] stmts]) of
    Program [proc] -> procBody proc
    Program _ -> stmts
