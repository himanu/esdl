import esdl.rand;
import std.stdio;

void main()
{
  writeln(constraintXlate(q{
	foreach (i, x; foo) {
	  foo[i] == 0;
	}
      }));
  
}
