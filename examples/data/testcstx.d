import esdl.data.cstx;

enum string CST=q{
    foreach(i, f; bar) f <= i;

    foreach(i, f; foo) if(i < 6) f == 24;
    
    foreach(i, f; foo) {
      f < 64;
      f > 16;
    }
    
};

  // q{
//   // ewfjewjofew
//   // ewfoewfjew

//   ff. // this is fun
//   length . /**/
//   bainsa < 43;
//   gg > 64;
  
//   foreach(i, f; foo) {
//     ((f < 64));
//     f > 16;
//   }
    
//   foreach(j, g; bar) {
//     g <= 16;
//     g >= 0;
//   }
// }

void main()
{
  auto parser = ConstraintParser(CST);
  // auto parser2 = parser.exprParser(60);
  auto test = parser.translate();
  import std.stdio;
  writeln(test);
}
