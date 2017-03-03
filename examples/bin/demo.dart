// Copyright 2017 Google Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

import 'dart:math';
import 'util.dart';
import 'package:propcheck/propcheck.dart';
import 'package:enumerators/combinators.dart';

List<int> countingSort(List<int> input) {
  var maximum = input.reduce(max);
  var counts = new List.filled(maximum + 1, 0);
  for (var i in input) {
    counts[i] += 1;
  }
  var result = new List();
  for (var i = 0; i < counts.length; i++) {
    if (counts[i] != 0) {
      result.add(i);
    }
  }
  return result;
}

void main() {
  assert(sameLists([1,2,5], countingSort([2,5,1])));
}
