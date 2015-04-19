-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Main where

import Init
import qualified CursesUI (UIM)
import CursesUIMInstance ()
import MainState

main = main'
    (Nothing :: (Maybe (CursesUI.UIM MainState -> IO (Maybe MainState))))
    (Just (doUI::CursesUI.UIM MainState -> IO (Maybe MainState)))
