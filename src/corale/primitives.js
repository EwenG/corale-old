goog.provide('corale.primitives');

corale.primitives.compareObj = function(comparator, a, b) {
  for (var k in a) {
    if (!(k in b) || !comparator(a[k], b[k])) {
      return false;
    }
  }
  for (var k in b) {
    if (!(k in a)) {
      return false;
    }
  }
  return true;
};
