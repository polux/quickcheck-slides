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

// Copyright (c) 2015, <your name>. All rights reserved. Use of this source code
// is governed by a BSD-style license that can be found in the LICENSE file.

library tree;

import 'dart:math';

abstract class Tree {
  int get size;
  int get depth;
}

class Branch implements Tree {
  final Tree left;
  final Tree right;

  Branch(this.left, this.right);

  @override
  String toString() => "B($left, $right)";

  @override
  int get size => 1 + left.size + right.size;

  @override
  int get depth => 1 + max(left.depth, right.depth);
}

class Leaf implements Tree {
  @override
  String toString() => "L";

  @override
  int get size => 0;

  @override
  int get depth => 0;
}
