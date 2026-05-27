{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.Tile
--
-- Backwards-compatible statement-list tiling entrypoint backed by the
-- polyhedral identity-order tiler.
module Language.Hydrangea.Tile
  ( tileStmts
  ) where

import Language.Hydrangea.CFG
import Language.Hydrangea.Polyhedral (polyhedralIdentityTileProgram)

tileStmts :: [Stmt] -> [Stmt]
tileStmts stmts =
  case polyhedralIdentityTileProgram (Program [mkProc "tile_stmts2" [] stmts]) of
    Program [proc] -> procBody proc
    Program _ -> stmts
