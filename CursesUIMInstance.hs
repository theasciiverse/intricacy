-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module CursesUIMInstance () where

import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH
import Control.Concurrent.STM
import Control.Concurrent
import Control.Applicative
import Data.Char (ord)
import Data.Monoid
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Array
import Data.Maybe
import Data.List
import Data.Char (chr)
import Control.Monad.Trans.Maybe
import Control.Concurrent (threadDelay)
import Control.Monad.State
import Data.Function (on)
import Data.Foldable (for_)

import Hex
import GameStateTypes
import Physics
import Frame
import Command
import KeyBindings
import ServerAddr
import Cache
import Database
import Metagame
import Protocol
import Util
import InputMode
import MainState
import CursesUI
import CursesRender
import CVec


drawName :: Bool -> CVec -> Codename -> MainStateT UIM ()
drawName showScore pos name = do
    ourName <- (authUser <$>) <$> gets curAuth
    relScore <- getRelScore name
    let (attr,col) = case relScore of
            Just 0 -> (a0,yellow)
            Just 1 -> (bold,cyan)
            Just 2 -> (a0,green)
            Just 3 -> (bold,green)
            Just (-1) -> (bold,magenta)
            Just (-2) -> (a0,red)
            Just (-3) -> (bold,red)
            _ -> if ourName == Just name then (bold,white) else (a0,white)
    lift $ drawStrCentred attr col pos
        (name ++ if showScore then " " ++ maybe "" show relScore else "")

drawActiveLock :: CVec -> ActiveLock -> MainStateT UIM ()
drawActiveLock pos al@(ActiveLock name i) = do
    accessed <- accessedAL al
    drawNameWithChar pos name
        (if accessed then green else white)
        (lockIndexChar i)

drawNameWithChar :: CVec -> Codename -> ColPair -> Char -> MainStateT UIM ()
drawNameWithChar pos name charcol char = do
    drawName False (pos +^ CVec 0 (-1)) name
    lift $ drawStr bold charcol (pos +^ CVec 0 1) [':',char]

drawNote :: CVec -> NoteInfo -> MainStateT UIM ()
drawNote pos note = case noteBehind note of
    Just al -> drawActiveLock pos al
    Nothing -> drawPublicNote pos (noteAuthor note)
    where
        drawPublicNote pos name =
            drawNameWithChar pos name magenta 'P'


fillBox :: CVec -> CVec -> Int -> Gravity -> [CVec -> MainStateT UIM ()] -> MainStateT UIM Int
fillBox (CVec t l) (CVec b r) width grav draws = do
    let half = width`div`2
        starty = (if grav == GravDown then b else t)
        cv = (b+t)`div`2
        ch = (l+r)`div`2
        gravCentre = case grav of
            GravDown   -> CVec b ch
            GravUp     -> CVec t ch
            GravLeft   -> CVec cv l
            GravRight  -> CVec cv r
            GravCentre -> CVec cv ch
        locs = sortBy (compare `on` dist) $ concat
            [ map (CVec j) [ l+margin + (width+1)*i | i <- [0..(r-l-(2*margin))`div`(width+1)] ]
            | j <- [t..b]
            , let margin = if (j-starty)`mod`2 == 0 then half else width ]
        dist v = sqlen $ v -^ gravCentre
        sqlen (CVec y x) = (y*(width+1))^2+x^2
    selDraws <- do
            offset <- gets listOffset
            let na = length locs
                nd = length draws
            return $ drop (max 0 $ min (nd - na) (na*offset)) $ draws
    let zipped = zip locs selDraws
    sequence_ $ map (uncurry ($)) $ zip selDraws locs
    return $ (if grav==GravDown then (minimum.(b:)) else (maximum.(t:))) [ y | (CVec y x,_) <- zipped ]

drawLockInfo al@(ActiveLock name i) lockinfo = do
    (h,w) <- liftIO Curses.scrSize
    let [left,vcentre,right] = [ (k+2*i)*w`div`6 + (1-k) | k <- [0,1,2] ]
    let [top,bottom] = [6, h-1]
    let hcentre = (top+bottom)`div`2 - 1
    ourName <- (authUser <$>) <$> gets curAuth

    (lockTop, lockBottom) <- (fromJust<$>)$ runMaybeT $ msum
        [ do
            lock <- mgetLock $ lockSpec lockinfo
            let size = frameSize $ fst lock
            guard $ bottom - top >= 5 + 2*size+1 + 1 + 5 && right-left >= 4*size+1
            lift.lift $ drawStateWithGeom [] False Map.empty (snd lock) (CVec hcentre vcentre,origin)
            return (hcentre - size - 1, hcentre + size + 1)
        , lift $ do
            drawActiveLock (CVec hcentre vcentre) al
            return (hcentre - 1, hcentre + 1)
        ]

    startOn <-
        if (public lockinfo)
        then lift $ drawStrCentred bold white (CVec (lockTop-1) vcentre) "Everyone!"
            >> return (lockTop-1)
        else if null $ accessedBy lockinfo
            then lift $ drawStrCentred a0 white (CVec (lockTop-1) vcentre) "No-one"
                >> return (lockTop-1)
            else
                fillBox (CVec (top+1) (left+1)) (CVec (lockTop-1) (right-1)) 5 GravDown $
        [ \pos -> drawNote pos note | note <- lockSolutions lockinfo ] ++
        [ \pos -> drawName False pos name
                    | name <- accessedBy lockinfo \\ map noteAuthor (lockSolutions lockinfo) ]
    lift $ drawStrCentred a0 white (CVec (startOn-1) vcentre) "Accessed by:"

    undecls <- gets undeclareds
    if isJust $ guard . (|| public lockinfo) . (`elem` accessedBy lockinfo) =<< ourName
    then lift $ drawStrCentred a0 green (CVec (lockBottom+1) vcentre) "Accessed!"
    else if any (\(Undeclared _ ls _) -> ls == lockSpec lockinfo) undecls
    then lift $ drawStrCentred a0 yellow (CVec (lockBottom+1) vcentre) "Undeclared solution!"
    else do
        read <- take 3 <$> getNotesReadOn lockinfo
        unless (null read || ourName == Just name) $ do
            let rntext = if right-left > 30 then "Read notes by:" else "Notes:"
                s = vcentre - (length rntext+(3+1)*3)`div`2
            lift $ drawStr a0 white (CVec (lockBottom+1) s) rntext
            void $ fillBox (CVec (lockBottom+1) (s+length rntext+1)) (CVec (lockBottom+1) right) 3 GravLeft
                [ \pos -> drawName False pos name | name <- map noteAuthor read ]

    lift $ drawStrCentred a0 white (CVec (lockBottom+2) vcentre) "Notes held:"
    if null $ notesSecured lockinfo
        then lift $
            drawStrCentred a0 white (CVec (lockBottom+3) vcentre) "None"
        else
            void $ fillBox (CVec (lockBottom+3) (left+1)) (CVec bottom (right-1)) 5 GravUp
                [ (`drawActiveLock` al) | al <- map noteOn $ notesSecured lockinfo ]


data HelpReturn = HelpNone | HelpDone | HelpContinue Int

showHelpPaged :: Int -> InputMode -> HelpPage -> UIM Bool
showHelpPaged from mode page =
    showHelpPaged' from mode page >>= \ret -> case ret of
        HelpNone -> return False
        HelpDone -> return True
        HelpContinue from' -> do
            drawPrompt False "[MORE]"
            getInput IMTextInput
            showHelpPaged from' mode page
showHelpPaged' :: Int -> InputMode -> HelpPage -> UIM HelpReturn
showHelpPaged' from mode HelpPageInput = do
    bdgs <- nub <$> getBindings mode
    erase
    (h,w) <- liftIO Curses.scrSize
    let bdgWidth = 39
        showKeys chs = intercalate "/" (map showKey chs)
        maxkeyslen = maximum $ map (length.showKeys.map fst) $ groupBy ((==) `on` snd) bdgs
    drawStrCentred a0 cyan (CVec 0 (w`div`2)) "Bindings:"
    let groups = filter (not . null . describeCommand . snd . head) $
            drop from $ groupBy ((==) `on` snd) $ sortBy (compare `on` snd) bdgs
    let draws =
            [ drawStr a0 cyan (CVec (y+2) (x*bdgWidth) ) $
                keysStr ++ replicate pad ' ' ++ ": " ++ desc
            | ((keysStr,pad,desc),(x,y)) <- zip
                [ (keysStr,pad,desc)
                | group <- groups
                , let cmd = snd $ head group
                , let desc = describeCommand cmd
                , let chs = map fst group
                , let keysStr = showKeys chs
                , let pad = max 0 $ minimum [maxkeyslen + 1 - length keysStr,
                        bdgWidth - length desc - length keysStr - 1 - 1]
                ]
                (map (`divMod` (h-3)) [0..])
            , (x+1)*bdgWidth < w]
    sequence_ draws
    refresh
    return $ if length draws < length groups
        then HelpContinue $ from + length draws
        else HelpDone
showHelpPaged' from IMMeta HelpPageGame =
    drawBasicHelpPage from ("INTRICACY",magenta) (metagameHelpText,magenta)
showHelpPaged' from IMMeta (HelpPageInitiated n) =
    drawBasicHelpPage from ("Initiation complete",magenta) (initiationHelpText n,magenta)
showHelpPaged' from IMEdit HelpPageFirstEdit =
    drawBasicHelpPage from ("Your first lock:",magenta) (firstEditHelpText,green)
showHelpPaged' _ _ _ = return HelpNone

drawBasicHelpPage :: Int -> (String,ColPair) -> ([String],ColPair) -> UIM HelpReturn
drawBasicHelpPage from (title,titleCol) (body,bodyCol) = do
    erase
    (h,w) <- liftIO Curses.scrSize
    drawStrCentred a0 titleCol (CVec 0 $ w`div`2) title
    let strs = drop from $
            if w >= maximum (map length metagameHelpText)
            then body
            else
                let wrap max = wrap' max max
                    wrap' _ _ [] = []
                    wrap' max left (w:ws) = if 1+length w > left
                        then if left == max
                            then take max w ++ "\n" ++
                                wrap' max max (drop max w : ws)
                            else '\n' : wrap' max max (w:ws)
                        else let prepend = if left == max then w else ' ':w
                            in prepend ++ wrap' max (left - length prepend) ws
                in lines . wrap w . words $ intercalate " " body
    let draws = [drawStrCentred a0 bodyCol (CVec y $ w`div`2) str |
                (y,str) <- zip [2..h-2] $ strs ]
    sequence_ draws
    return $ if length draws < length strs
        then HelpContinue $ from + length draws
        else HelpDone


charify :: Curses.Key -> Maybe Char
charify key = case key of
    Curses.KeyChar ch -> Just ch
    Curses.KeyBackspace -> Just '\b'
    Curses.KeyLeft -> Just '4'
    Curses.KeyRight -> Just '6'
    Curses.KeyDown -> Just '2'
    Curses.KeyUp -> Just '8'
    Curses.KeyHome -> Just '7'
    Curses.KeyNPage -> Just '3'
    Curses.KeyPPage -> Just '9'
    Curses.KeyEnd -> Just '1'
    _ -> Nothing

handleEsc k@(Curses.KeyChar '\ESC') = do
    Curses.timeout 100
    cch <- Curses.getch
    Curses.timeout (-1)
    return $ if cch == -1 then k
        else Curses.KeyChar $ chr $ fi cch+128
handleEsc k = return k

instance UIMonad (StateT UIState IO) where
    runUI m = evalStateT m nullUIState

    drawMainState = do
        lift erase
        s <- get
        lift . drawTitle =<< getTitle
        lift drawMsgLine
        drawMainState' s
        lift refresh
        where
        drawMainState' (PlayState { psCurrentState=st, psLastAlerts=alerts,
                wrenchSelected=wsel, psFrame=frame }) = lift $ do
            drawState [] False alerts st
            drawBindingsTables IMPlay frame
            drawCursorAt $ listToMaybe [ pos |
                (_, PlacedPiece pos p) <- enumVec $ placedPieces st
                , or [wsel && isWrench p, not wsel && isHook p] ]
        drawMainState' (ReplayState {}) = do
            lift . drawState [] False [] =<< gets rsCurrentState
            lift $ drawCursorAt Nothing
        drawMainState' (EditState { esGameStateStack=(st:_), selectedPiece=selPiece,
                selectedPos=selPos, esFrame=frame }) = lift $ do
            drawState (maybeToList selPiece) True [] st
            drawBindingsTables IMEdit frame
            drawCursorAt $ if isNothing selPiece then Just selPos else Nothing
        drawMainState' (MetaState {curServer=saddr, undeclareds=undecls,
                cacheOnly=cOnly, curAuth=auth, codenameStack=names,
                randomCodenames=rnamestvar, retiredLocks=mretired, curLockPath=path,
                curLock=lock}) = do
            let ourName = liftM authUser auth
            let selName = listToMaybe names
            let home = isJust ourName && ourName == selName
            (h,w) <- liftIO Curses.scrSize
            when (h<20 || w<40) $ liftIO CursesH.end >> error "Terminal too small!"
            bdgs <- lift $ getBindings IMMeta
            lift $ do
                drawCursorAt Nothing
                let serverBdgsDraw = bindingsDraw bdgs
                        [CmdSetServer, CmdToggleCacheOnly]
                    lockBdgsDraw = bindingsDraw bdgs $
                        [CmdEdit] ++ if path == "" then [] else [CmdPlaceLock Nothing]
                    leftBdgsWidth = (+3) . maximum $ map drawWidth [serverBdgsDraw, lockBdgsDraw]
                    helpDraw = bindingsDraw bdgs [CmdTutorials] <> greyDraw " tutorial  " <>
                        bindingsDraw bdgs [CmdHelp] <> greyDraw " help"
                    serverTextDraw = greyDraw . take (w - leftBdgsWidth - drawWidth helpDraw - 1) $
                        " Server: " ++ saddrStr saddr ++ (if cOnly then "  (offline mode) " else "")
                    lockBdgsDraw' = bindingsDraw bdgs $
                        [CmdSelectLock] ++ if path == "" then [] else [CmdNextLock, CmdPrevLock]
                    lockTextDraw = greyDraw . take (w - leftBdgsWidth - drawWidth lockBdgsDraw' - 1) $
                        " Lock: " ++ path ++ replicate 5 ' '
                doDrawAt (CVec 0 0) $ alignDraw GravLeft leftBdgsWidth serverBdgsDraw <> serverTextDraw
                doDrawAt (CVec 0 0) $ alignDraw GravRight w helpDraw
                doDrawAt (CVec 1 0) $ alignDraw GravLeft leftBdgsWidth lockBdgsDraw <> lockTextDraw <> lockBdgsDraw'

                doDrawAt (CVec 2 $ maximum [w`div`3+1, w`div`2 - 13]) $ bindingsDraw bdgs [CmdSelCodename Nothing]

            maybe (return ()) (drawName True (CVec 2 (w`div`2))) selName
            void.runMaybeT $ MaybeT (return selName) >>= lift . getUInfoFetched 300 >>=
                \(FetchedRecord fresh err muirc) -> lift $ do
                    lift $ do
                        unless fresh $ drawAtCVec (Glyph '*' red bold) $ CVec 2 (w`div`2+7)
                        maybe (return ()) sayError err
                        when (fresh && (isNothing ourName || home || isNothing muirc)) $
                            doDrawAt (CVec 2 (w`div`2+1+9)) $
                                bindingsDraw bdgs $
                                    if (isNothing muirc && isNothing ourName) || home
                                    then [CmdRegister $ isJust ourName] else [CmdAuth]
                    for_ muirc $ \(RCUserInfo (_,uinfo)) -> case mretired of
                        Just retired -> do
                            (h,w) <- liftIO Curses.scrSize
                            void $ fillBox (CVec 6 2) (CVec (h-1) (w-2)) 5 GravCentre
                                [ \pos -> lift $ drawStrGrey pos $ show ls | ls <- retired ]
                            lift $ doDrawAt (CVec 5 (w`div`3)) $ bindingsDraw bdgs $
                                [CmdShowRetired] ++ if null retired
                                then [] else [CmdPlayLockSpec Nothing]
                        Nothing -> do
                            sequence_ [ drawLockInfo (ActiveLock (codename uinfo) i) lockinfo |
                                (i,Just lockinfo) <- assocs $ userLocks uinfo ]
                            unless (null $ elems $ userLocks uinfo) $ lift $
                                doDrawAt (CVec 5 (w`div`3)) $ bindingsDraw bdgs $
                                [CmdSolve Nothing] ++ if isJust ourName then [CmdViewSolution Nothing] else []
            when (isJust ourName && ourName == selName) $ do
                rnames <- liftIO $ atomically $ readTVar rnamestvar
                unless (null rnames) $
                    void $ fillBox (CVec 2 0) (CVec 5 (w`div`3)) 3 GravCentre
                        [ \pos -> drawName False pos name | name <- rnames ]
                unless (null undecls) $
                    let declareBdgDraw = bindingsDraw bdgs [CmdDeclare Nothing]
                        declareText = " Undeclared solutions:"
                        y = 4
                        leftBound = w`div`3 + 1
                        undeclsWidth = 1 + 6 * length undecls
                        declareDraw =
                            if leftBound + drawWidth declareBdgDraw + length declareText + undeclsWidth >= w
                            then declareBdgDraw
                            else declareBdgDraw <> stringDraw bold white declareText
                        width = drawWidth declareDraw + undeclsWidth
                        left = max leftBound ((w - width) `div` 2)
                    in do
                        lift $ doDrawAt (CVec y left) declareDraw
                        void $ fillBox
                            (CVec y $ left + drawWidth declareDraw + 1)
                            (CVec y (w-1)) 5 GravLeft
                            [ (`drawActiveLock` al) | Undeclared _ _ al <- undecls ]

            when (ourName /= selName) $ void $ runMaybeT $ do
                sel <- liftMaybe selName
                us <- liftMaybe ourName
                ourUInfo <- mgetUInfo us
                let accessed = [ ActiveLock us i
                        | i<-[0..2]
                        , Just lock <- [ userLocks ourUInfo ! i ]
                        , public lock || selName `elem` map Just (accessedBy lock) ]
                guard $ not $ null accessed
                let str = "has accessed:"
                let s = (w-(4 + length str + 6*(length accessed)))`div`2
                let y = 4
                lift $ do
                    drawName False (CVec y (s+1)) sel
                    lift $ drawStrGrey (CVec y $ s+4) str
                    void $ fillBox (CVec y (s+4+length str+1)) (CVec y (w-1)) 5 GravLeft $
                        [ (`drawActiveLock` al) | al <- accessed]

    reportAlerts _ alerts =
        do mapM_ drawAlert alerts
           unless (null alerts)
            $ do refresh
                 liftIO $ threadDelay $ 5*10^4
        where
            drawAlert (AlertCollision pos) = drawAt cGlyph pos
            drawAlert _ = return ()
            cGlyph = Glyph '!' 0 a0

    drawMessage = say
    drawPrompt full s = say s >> (liftIO $ void $ Curses.cursSet Curses.CursorVisible)
    endPrompt = say "" >> (liftIO $ void $ Curses.cursSet Curses.CursorInvisible)
    drawError = sayError

    showHelp = showHelpPaged 0

    getChRaw = (charify<$>) $ liftIO $ CursesH.getKey (return ()) >>= handleEsc
    setUIBinding mode cmd ch =
        modify $ \s -> s { uiKeyBindings =
                Map.insertWith (\[bdg] -> \bdgs -> if bdg `elem` bdgs then delete bdg bdgs else bdg:bdgs)
                    mode [(ch,cmd)] $ uiKeyBindings s }
    getUIBinding mode cmd = do
        bdgs <- getBindings mode
        return $ maybe "" showKey $ findBinding bdgs cmd

    initUI = do
        liftIO CursesH.start
        cpairs <- liftIO $ colorsToPairs [ (f, CursesH.black)
            | f <- [ CursesH.white, CursesH.red, CursesH.green, CursesH.yellow
                , CursesH.blue, CursesH.magenta, CursesH.cyan] ]
        modify $ \s -> s {dispCPairs = cpairs}
        readBindings
        return True

    endUI = do
        writeBindings
        liftIO CursesH.end
    unblockInput = return $ Curses.ungetCh 0
    suspend = do
        liftIO $ do
            CursesH.suspend
            Curses.resetParams
        redraw
    redraw = liftIO $ do
        Curses.endWin
        Curses.refresh

    warpPointer _ = return ()
    getUIMousePos = return Nothing
    setYNButtons = return ()
    onNewMode _ = say ""
    withNoBG = id

    toggleColourMode = modify $ \s -> s {monochrome = not $ monochrome s}

    impatience ticks = do
        when (ticks>20) $ say "Waiting for server (^C to abort)..."
        unblock <- unblockInput
        liftIO $ forkIO $ threadDelay 50000 >> unblock
        cmds <- getInput IMImpatience
        return $ CmdQuit `elem` cmds

    getInput mode = do
        let userResizeCode = 1337  -- XXX: chosen not to conflict with HSCurses codes
        key <- liftIO $ CursesH.getKey (Curses.ungetCh userResizeCode) >>=
            handleEsc
        if key == Curses.KeyUnknown userResizeCode
            then do
                liftIO Curses.scrSize
                return [CmdRedraw]
            else do
                let mch = charify key
                    unblockBinding = (toEnum 0, CmdRefresh) -- c.f. unblockInput above
                flip (maybe $ return []) mch $ \ch ->
                    if mode == IMTextInput
                    then return $ [ CmdInputChar ch `fromMaybe` lookup ch [unblockBinding] ]
                    else (maybeToList . lookup ch . (unblockBinding:)) <$> getBindings mode
