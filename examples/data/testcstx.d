import esdl.data.cstx;
enum string CST="foo.length == 8;\nforeach(_esdl__elem_foo_0; foo) {\n_esdl__elem_foo_0.length == 4;\n}"
;

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
  auto parser = CstParser(CST);
  // auto parser2 = parser.exprParser(60);
  auto test = parser.translate();
  import std.stdio;
  writeln(test);
}
