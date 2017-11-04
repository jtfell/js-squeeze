function longfnname(a) {
  // Should combine decls
  var longvarname = a + true;
  var x;
  var y;
  return longvarname;
  // Should drop statements after a return
  var p = "hey";
  console.log(p);
}

// Should mangle fn and var names
longfnname(22);
