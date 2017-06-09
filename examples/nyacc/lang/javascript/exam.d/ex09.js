var x = 1;
try {
  x = 2; throw 1;
  x = 3;
} catch (e) { 
  display("caught "); display(e); newline();
} finally { 
  display("finally "); display(x); newline();
}
