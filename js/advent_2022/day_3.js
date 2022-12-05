function sum(a) {
  return a && a.reduce(function(acc, v) {
    return acc + v;
  },
  0);
}

function conj(a, v) {
  return a && a.concat([v]);
}

function partition(a, n) {
  return a && a.reduce(function(acc, v) {
    let [res, part, c] = acc;
    if (c === n) {
      return [conj(res, conj(part, v)), [], 1];
    } else {
      return [res, conj(part, v), ++c];
    }
  },
  [[], [], 1]).shift();
}

function mapcat(f, coll) {
  return [].concat.apply([], coll.map(f));
}

function range(start, stop) {
  return Array(stop - start).fill(start).map((x, y) => x + y);
}

function intersection(sets) {
  let [f, ...rest] = sets;
  return new Set([...f].filter(function(v) {
    return rest.every(function(s) {
      return s.has(v);
    })
  }));
}

function parse(input) {
  return input.split("\n").map(l => new Set(l));
}

const priority = Object.fromEntries(range(97, 123)
  .concat(range(65, 91))
  .map(function(v, idx) {
    return [String.fromCharCode(v), ++idx];
  }));

function part2(input) {
  let parsed = parse(input),
    partitioned = partition(parsed, 3),
    badges = mapcat(function(part) {
      return Array.from(intersection(part));
    }, partitioned),
    priorities = badges.map(v => priority[v]);

  return sum(priorities);
}

console.log(sum([1, 2, 3]));

console.log(partition([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12], 3));

console.log(intersection([new Set([1, 2, 3]), new Set([2, 3, 4]), new Set([3, 4, 5])]));

console.log(part2("vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"));
