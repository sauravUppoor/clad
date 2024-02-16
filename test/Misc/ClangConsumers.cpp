// RUN: %cladclang %s -I%S/../../include -oClangConsumers.out -Xclang -print-stats 2>&1 | FileCheck %s
// CHECK-NOT: {{.*error|warning|note:.*}}

#include "clad/Differentiator/Differentiator.h"
// CHECK: HandleTopLevelDecl
int main() {

}
