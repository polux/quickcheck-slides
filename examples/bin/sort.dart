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
import 'package:collection/collection.dart';
import 'package:propcheck/propcheck.dart';
import 'package:enumerators/enumerators.dart' as E;
import 'package:enumerators/combinators.dart' as E;

var listEquality = const ListEquality(const DefaultEquality());
bool sameLists(List l1, List l2) => listEquality.equals(l1, l2);

List<int> dartSort(List<int> input) {
  return input.toList()..sort();
}

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

List<int> countingSort2(List<int> input) {
  if (input.isEmpty)
    return [];
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

List<int> countingSort3(List<int> input) {
  if (input.isEmpty) return [];
  var maximum = input.reduce(max); 
  var minimum = input.reduce(min);
  var counts = new List.filled(maximum - minimum + 1, 0);
  for (var i in input) {
    counts[i-minimum] += 1;
  }
  var result = new List();
  for (var i = 0; i < counts.length; i++) {
    if (counts[i] != 0) {
      result.add(i+minimum);
    }
  }
  return result;
}

List<int> countingSort4(List<int> input) {
  if (input.isEmpty) return [];
  var maximum = input.reduce(max); 
  var minimum = input.reduce(min);
  var counts = new List.filled(maximum - minimum + 1, 0);
  for (var i in input) {
    counts[i-minimum] += 1;
  }
  var result = new List();
  for (var i = 0; i < counts.length; i++) {
    var count = counts[i];
    for (int j = 0; j < count; j++) {
      result.add(i+minimum);
    }
  }
  return result;
}

bool correct(List<int> list) {
  return sameLists(countingSort4(list), dartSort(list));
}

void _main() {
  print(sameLists(countingSort([42]), [42])); // true
  print(sameLists(countingSort([2,5,1]), [1,2,5])); // true
}

void main() {
  new QuickCheck().check(forall(E.listsOf(E.ints), correct));
}
