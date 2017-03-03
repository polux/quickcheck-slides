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

library util;

import 'package:collection/collection.dart';
import 'package:propcheck/propcheck.dart';
import 'package:enumerators/combinators.dart';

var listEquality = const ListEquality(const DefaultEquality());
bool sameLists(List l1, List l2) => listEquality.equals(l1, l2);

List<int> dartSort(List<int> input) {
  return input.toList()..sort();
}

void checkForManyLists(Function f) {
  new QuickCheck().check(forall(listsOf(ints), f));
}


