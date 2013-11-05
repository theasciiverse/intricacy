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
