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

-- TODO: High-level file comment.
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

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

myBars vals = liftEC $ do
  style <- fmap mkStyle takeColor
  plot_bars_values .= vals
  plot_bars_style .= BarsClustered
  plot_bars_spacing .= BarsFixGap 30 5
  plot_bars_item_styles .= [style]
    where
      mkStyle c = (solidFillStyle c, Just (solidLine 1.0 $ opaque black))

dump filename values = toFile (fo_size .~ (1024, 768) $ def) filename $ do
  layout_title .= "10,000 Random Trees - Size distribution"
  layout_all_font_styles . font_size .= 14
  layout_title_style . font_size .= 16
  --layout_x_axis . laxis_title .= "size"
  --layout_y_axis . laxis_title .= "number of trees"
  layout_x_axis . laxis_generate .= autoIndexAxis (map fst values)
  plot $ fmap plotBars $ myBars (addIndexes (map snd values))

dumpPoints filename values = toFile (def & fo_size .~ (1024, 768)) filename $ do
  layout_title .= "10,000 trees of size 100"
  layout_x_axis . laxis_title .= "depth"
  layout_y_axis . laxis_title .= "label value"
  plot $ liftEC $ do
    area_spots_values .= values

load f = read `fmap` readFile f

loadBars :: String -> IO [(String, [Double])]
loadBars = load

loadPoints :: String -> IO [(Int, Int, Int)]
loadPoints = load

out n = "../slides/out/img/gen" ++ show n ++ ".png"
outPoints name = "../slides/out/img/" ++ name ++ ".png"


main = do
  gen1 <- loadBars "gen1"
  gen2 <- loadBars "gen2"
  gen3 <- loadBars "gen3"
  gen4 <- loadBars "gen4"
  gen5 <- loadBars "gen5"
  dump (out 1) gen1
  dump (out 6) (take 50 gen1)
  dump (out 2) gen2
  dump (out 3) gen3
  dump (out 4) gen4
  dump (out 5) gen5

  pointsBad <- loadPoints "size_by_depth_bad"
  dumpPoints (outPoints "size_by_depth_bad") pointsBad
  pointsGood <- loadPoints "size_by_depth_good"
  dumpPoints (outPoints "size_by_depth_good") pointsGood
