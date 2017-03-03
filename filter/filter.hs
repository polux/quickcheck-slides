-- Copyright 2017 Google Inc.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- compile with:  ghc --make Pygments
-- run with:      pandoc -t json test.md | ./Pygments | pandoc -f json -s -H pygments.header > test.html

import Text.Pandoc
import System.Process
import Text.Pandoc.JSON (toJSONFilter)

main = toJSONFilter hilight
  where hilight (CodeBlock (_,[lang], [("hl_lines", hlLines)]) code) = RawBlock (Format "html") `fmap` pygments code lang hlLines
        hilight (CodeBlock (_,[lang], _) code) = RawBlock (Format "html") `fmap` pygments code lang ""
        hilight x = return x

{-
css :: IO String
css = readProcess "pygmentize" ["-S", "default",  "-f", "html"]
-}

pygments:: String -> String -> String -> IO String
pygments code lang hlLines = readProcess "pygmentize" ["-l", lang,  "-f", "html", "-Phl_lines="++hlLines] code
