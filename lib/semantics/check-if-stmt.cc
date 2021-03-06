// Copyright (c) 2019, NVIDIA CORPORATION.  All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "check-if-stmt.h"
#include "tools.h"
#include "../parser/message.h"
#include "../parser/parse-tree.h"

namespace Fortran::semantics {

void IfStmtChecker::Leave(const parser::IfStmt &ifStmt) {
  // C1143 Check that the action stmt is not an if stmt
  const auto &body{
      std::get<parser::UnlabeledStatement<parser::ActionStmt>>(ifStmt.t)};
  if (std::holds_alternative<common::Indirection<parser::IfStmt>>(
          body.statement.u)) {
    context_.Say(
        body.source, "IF statement is not allowed in IF statement"_err_en_US);
  }
}

}  // namespace Fortran::semantics
