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

import 'package:examples/tree.dart';
import 'dart:math';
import 'dart:io';

final Random r = new Random();

Tree gen1() {
  if (r.nextBool()) {
    return new Leaf();
  } else {
    return new Branch(gen1(), gen1());
  }
}

Tree gen2() {
  if (r.nextInt(5) == 0) {
    return new Leaf();
  } else {
    return new Branch(gen2(), gen2());
  }
}

Tree gen3(depth) {
  if (depth == 0) {
    return new Leaf();
  } else {
    if (r.nextInt(5) == 0) {
      return new Leaf();
    } else {
      return new Branch(gen3(depth - 1), gen3(depth - 1));
    }
  }
}

Tree gen4(n) {
  if (n == 0) {
    return new Leaf();
  }
  if (r.nextInt(5) == 0) {
    return new Leaf();
  } else {
    return new Branch(gen4(n ~/ 2), gen4(n ~/ 2));
  }
}

Tree gen5(n) {
  if (n == 0) {
    return new Leaf();
  } else {
    final p = r.nextInt(n);
    return new Branch(gen5(p), gen5(n - 1 - p));
  }
}


int median(List xs) {
  final sorted = xs.toList()..sort();
  return sorted[sorted.length ~/ 2];
}

void run(file, gen, n) {
  int stackOverflows = 0;
  List<int> sizes = [];
//  List<int> depths = [];

  for (int i = 0; i < n; i++) {
    stderr.writeln(i);
    try {
      final Tree t = gen();
      sizes.add(t.size);
//      sizes.add(t.depth);
    } catch (e) {
      stackOverflows++;
    }
  }

  int maxSize = sizes.reduce(max);
  final sizeMap = {};
  for (int i = 0; i < maxSize + 1; i++) {
    sizeMap[i] = 0;
  }
  for (final s in sizes) {
    sizeMap[s]++;
  }
  final res = [];
  for (int i = 0; i < maxSize + 1; i++) {
    res.add('("$i", [${sizeMap[i]}])');
  }
  if (stackOverflows > 0) {
    res.add('("∞", [$stackOverflows])');
  }
  new File(file).writeAsStringSync("[${res.join(',')}]");
}


main(List<String> arguments) {
  final n = 10000;
  file(n) => "../../histograms/gen$n";
  //run(file(1), gen1, n);
//  run(file(2), gen2, n);
//  run(file(3), () => gen3(5), n);
//  run(file(4), () => gen4(30), n);
  run(file(5), () => gen5(30), n);
}
