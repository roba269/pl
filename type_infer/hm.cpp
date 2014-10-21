#include "ast.h"
#include "types.h"
#include <iostream>
using namespace std;

int main() {
  const Exp& x = Apply(Ident("f"), Ident("5"));
  cout << x.show() << endl;
  return 0;
}
