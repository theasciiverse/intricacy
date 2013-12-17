-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

-- Functions which are no longer used anywhere

-- from Gamestate:
delConnectionsAt :: Locus -> GameState -> GameState
delConnectionsAt locus st =
    st {connections = filter
	(\conn -> locus `notElem` [connectionRoot conn, connectionEnd conn])
        $ connections st}
collidePieces :: PlacedPiece -> PlacedPiece -> [HexPos]
collidePieces pp pp' =
    intersect (plPieceFootprint pp) (plPieceFootprint pp')
piecesIntersect :: PlacedPiece -> PlacedPiece -> Bool
piecesIntersect pp pp' = not $ null $ collidePieces pp pp'
pieceIntersectsConnection :: GameState -> PlacedPiece -> Connection -> Bool
pieceIntersectsConnection s pp c =
    not $ null $ intersect (plPieceFootprint pp) (connectionFootPrint s c)
checkConnGraphAcyclicNaive :: GameState -> Bool
checkConnGraphAcyclicNaive st = and [ idx `notElem` descendents idx
	| idx <- [0..Vector.length $ placedPieces st] ]
    where descendents idx = let childs = map (fst.connectionRoot) $ springsEndAtIdx st idx
	    in concat $ (childs:map descendents childs) 


-- from SDLUI
drawPaintSel :: UIM ()
drawPaintSel = do
    pti <- getEffPaintTileIndex
    case (paintTiles!!pti) of
	Nothing -> return ()
	Just t -> renderToMain $ drawAtRel (tileGlyph t (dim $ colourWheel 5)) (periphery 0)
