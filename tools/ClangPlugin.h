//--------------------------------------------------------------------*- C++ -*-
// clad - the C++ Clang-based Automatic Differentiator
// version: $Id$
// author:  Vassil Vassilev <vvasilev-at-cern.ch>
//------------------------------------------------------------------------------

#ifndef CLAD_CLANG_PLUGIN
#define CLAD_CLANG_PLUGIN

#include "DerivedFnInfo.h"
#include "clad/Differentiator/DerivativeBuilder.h"
#include "clad/Differentiator/DiffMode.h"
#include "clad/Differentiator/DiffPlanner.h"
#include "clad/Differentiator/Version.h"

#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Basic/Version.h"
#include "clang/Frontend/FrontendPluginRegistry.h"
#include "clang/Frontend/MultiplexConsumer.h"
#include "clang/Sema/SemaConsumer.h"

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Timer.h"

namespace clang {
  class ASTContext;
  class CallExpr;
  class CompilerInstance;
  class DeclGroupRef;
  class Expr;
  class FunctionDecl;
  class ParmVarDecl;
  class Sema;
} // namespace clang

namespace clad {

  bool checkClangVersion();
  /// This class is designed to store collection of `DerivedFnInfo` objects.
  /// It's purpose is to avoid repeated generation of same derivatives by
  /// making it possible to reuse previously computed derivatives.
  class DerivedFnCollector {
    using DerivedFns = llvm::SmallVector<DerivedFnInfo, 16>;
    /// Mapping to efficiently find out information about all the derivatives of
    /// a function.
    llvm::DenseMap<const clang::FunctionDecl*, DerivedFns> m_DerivedFnInfoCollection;

  public:
    /// Adds a derived function to the collection.
    void Add(const DerivedFnInfo& DFI);

    /// Finds a `DerivedFnInfo` object in the collection that satisfies the
    /// given differentiation request.
    DerivedFnInfo Find(const DiffRequest& request) const;

    bool IsDerivative(const clang::FunctionDecl* FD) const;

  private:
    /// Returns true if the collection already contains a `DerivedFnInfo`
    /// object that represents the same derivative object as the provided
    /// argument `DFI`.
    bool AlreadyExists(const DerivedFnInfo& DFI) const;
  };
  class CladTimerGroup {
    llvm::TimerGroup m_Tg;
    std::vector<std::unique_ptr<llvm::Timer>> m_Timers;

  public:
    CladTimerGroup();
    void StartNewTimer(llvm::StringRef TimerName, llvm::StringRef TimerDesc);
    void StopTimer();
  };

  namespace plugin {
    struct DifferentiationOptions {
      DifferentiationOptions()
          : DumpSourceFn(false), DumpSourceFnAST(false), DumpDerivedFn(false),
            DumpDerivedAST(false), GenerateSourceFile(false),
            ValidateClangVersion(true), EnableTBRAnalysis(false),
            CustomEstimationModel(false), PrintNumDiffErrorInfo(false) {}

      bool DumpSourceFn : 1;
      bool DumpSourceFnAST : 1;
      bool DumpDerivedFn : 1;
      bool DumpDerivedAST : 1;
      bool GenerateSourceFile : 1;
      bool ValidateClangVersion : 1;
      bool EnableTBRAnalysis : 1;
      bool CustomEstimationModel : 1;
      bool PrintNumDiffErrorInfo : 1;
      std::string CustomModelName;
    };

    class CladPlugin : public clang::SemaConsumer {
      clang::CompilerInstance& m_CI;
      DifferentiationOptions m_DO;
      std::unique_ptr<DerivativeBuilder> m_DerivativeBuilder;
      bool m_HasRuntime = false;
      bool m_PendingInstantiationsInFlight = false;
      bool m_HandleTopLevelDeclInternal = false;
      CladTimerGroup m_CTG;
      DerivedFnCollector m_DFC;
      enum class CallKind {
        HandleCXXStaticMemberVarInstantiation,
        HandleTopLevelDecl,
        HandleInlineFunctionDefinition,
        HandleInterestingDecl,
        HandleTagDeclDefinition,
        HandleTagDeclRequiredDefinition,
        HandleCXXImplicitFunctionInstantiation,
        HandleTopLevelDeclInObjCContainer,
        HandleImplicitImportDecl,
        CompleteTentativeDefinition,
        CompleteExternalDeclaration,
        AssignInheritanceModel,
        HandleVTable,
        InitializeSema,
        ForgetSema
      };
      struct DelayedCallInfo {
        CallKind m_Kind;
        clang::DeclGroupRef m_DGR;
        DelayedCallInfo(CallKind K, clang::DeclGroupRef DGR)
            : m_Kind(K), m_DGR(DGR) {}
        DelayedCallInfo(CallKind K, const clang::Decl* D)
            : m_Kind(K), m_DGR(const_cast<clang::Decl*>(D)) {}
      };
      std::vector<DelayedCallInfo> m_DelayedCalls;
      std::unique_ptr<clang::MultiplexConsumer> m_Multiplexer;

    public:
      CladPlugin(clang::CompilerInstance& CI, DifferentiationOptions& DO);
      ~CladPlugin();
      // ASTConsumer
      void Initialize(clang::ASTContext& Context) override;
      void HandleCXXStaticMemberVarInstantiation(clang::VarDecl* D) override {
        AppendDelayed({CallKind::HandleCXXStaticMemberVarInstantiation, D});
        m_Multiplexer->HandleCXXStaticMemberVarInstantiation(D);
      }
      bool HandleTopLevelDecl(clang::DeclGroupRef D) override; /*{
        AppendDelayed({CallKind::HandleTopLevelDecl, D});
        return true; // happyness, continue parsing
        }*/
      void HandleInlineFunctionDefinition(clang::FunctionDecl* D) override {
        AppendDelayed({CallKind::HandleInlineFunctionDefinition, D});
        m_Multiplexer->HandleInlineFunctionDefinition(D);
      }
      void HandleInterestingDecl(clang::DeclGroupRef D) override {
        AppendDelayed({CallKind::HandleInterestingDecl, D});
        m_Multiplexer->HandleInterestingDecl(D);
      }
      void HandleTagDeclDefinition(clang::TagDecl* D) override {
        AppendDelayed({CallKind::HandleTagDeclDefinition, D});
        m_Multiplexer->HandleTagDeclDefinition(D);
      }
      void HandleTagDeclRequiredDefinition(const clang::TagDecl* D) override {
        AppendDelayed({CallKind::HandleTagDeclRequiredDefinition, D});
        m_Multiplexer->HandleTagDeclRequiredDefinition(D);
      }
      void
      HandleCXXImplicitFunctionInstantiation(clang::FunctionDecl* D) override {
        AppendDelayed({CallKind::HandleCXXImplicitFunctionInstantiation, D});
        m_Multiplexer->HandleCXXImplicitFunctionInstantiation(D);
      }
      void HandleTopLevelDeclInObjCContainer(clang::DeclGroupRef D) override {
        AppendDelayed({CallKind::HandleTopLevelDeclInObjCContainer, D});
        m_Multiplexer->HandleTopLevelDeclInObjCContainer(D);
      }
      void HandleImplicitImportDecl(clang::ImportDecl* D) override {
        AppendDelayed({CallKind::HandleImplicitImportDecl, D});
        m_Multiplexer->HandleImplicitImportDecl(D);
      }
      void CompleteTentativeDefinition(clang::VarDecl* D) override {
        AppendDelayed({CallKind::CompleteTentativeDefinition, D});
        m_Multiplexer->CompleteTentativeDefinition(D);
      }
#if CLANG_VERSION_MAJOR > 9
      void CompleteExternalDeclaration(clang::VarDecl* D) override {
        AppendDelayed({CallKind::CompleteExternalDeclaration, D});
        m_Multiplexer->CompleteExternalDeclaration(D);
      }
#endif
      void AssignInheritanceModel(clang::CXXRecordDecl* D) override {
        AppendDelayed({CallKind::AssignInheritanceModel, D});
        m_Multiplexer->AssignInheritanceModel(D);
      }
      void HandleVTable(clang::CXXRecordDecl* D) override {
        AppendDelayed({CallKind::HandleVTable, D});
        m_Multiplexer->HandleVTable(D);
      }

      // Not delayed.
      void HandleTranslationUnit(clang::ASTContext& C) override {
        m_Multiplexer->HandleTranslationUnit(C);
      }
      // No need to handle the listeners, they will be handled at non-delayed by
      // the parent multiplexer.
      //
      // clang::ASTMutationListener *GetASTMutationListener() override;
      // clang::ASTDeserializationListener *GetASTDeserializationListener()
      // override;
      void PrintStats() override { m_Multiplexer->PrintStats(); }
      bool shouldSkipFunctionBody(clang::Decl* D) override {
        return m_Multiplexer->shouldSkipFunctionBody(D);
      }

      // SemaConsumer
      void InitializeSema(clang::Sema& S) override {
        AppendDelayed({CallKind::InitializeSema, nullptr});
        m_Multiplexer->InitializeSema(S);
      }
      void ForgetSema() override {
        AppendDelayed({CallKind::ForgetSema, nullptr});
        m_Multiplexer->ForgetSema();
      }

      // bool HandleTopLevelDecl(clang::DeclGroupRef DGR) override;
      clang::FunctionDecl* ProcessDiffRequest(DiffRequest& request);

    private:
      void AppendDelayed(DelayedCallInfo DCI) { m_DelayedCalls.push_back(DCI); }
      bool CheckBuiltins();
      void ProcessTopLevelDecl(clang::Decl* D);
    };

    clang::FunctionDecl* ProcessDiffRequest(CladPlugin& P,
                                            DiffRequest& request) {
      return P.ProcessDiffRequest(request);
    }

    template <typename ConsumerType>
    class Action : public clang::PluginASTAction {
    private:
      DifferentiationOptions m_DO;

    protected:
      std::unique_ptr<clang::ASTConsumer>
      CreateASTConsumer(clang::CompilerInstance& CI,
                        llvm::StringRef InFile) override {
        return std::unique_ptr<clang::ASTConsumer>(new ConsumerType(CI, m_DO));
      }

      bool ParseArgs(const clang::CompilerInstance& CI,
                     const std::vector<std::string>& args) override {
        for (unsigned i = 0, e = args.size(); i != e; ++i) {
          if (args[i] == "-fdump-source-fn") {
            m_DO.DumpSourceFn = true;
          } else if (args[i] == "-fdump-source-fn-ast") {
            m_DO.DumpSourceFnAST = true;
          } else if (args[i] == "-fdump-derived-fn") {
            m_DO.DumpDerivedFn = true;
          } else if (args[i] == "-fdump-derived-fn-ast") {
            m_DO.DumpDerivedAST = true;
          } else if (args[i] == "-fgenerate-source-file") {
            m_DO.GenerateSourceFile = true;
          } else if (args[i] == "-fno-validate-clang-version") {
            m_DO.ValidateClangVersion = false;
          } else if (args[i] == "-enable-tbr") {
            m_DO.EnableTBRAnalysis = true;
          } else if (args[i] == "-fcustom-estimation-model") {
            m_DO.CustomEstimationModel = true;
            if (++i == e) {
              llvm::errs() << "No shared object was specified.";
              return false;
            }
            m_DO.CustomModelName = args[i];
          } else if (args[i] == "-fprint-num-diff-errors") {
            m_DO.PrintNumDiffErrorInfo = true;
          } else if (args[i] == "-help") {
            // Print some help info.
            llvm::errs()
                << "Option set for the clang-based automatic differentiator - "
                   "clad:\n\n"
                << "-fdump-source-fn - Prints out the source code of the "
                   "function.\n"
                << "-fdump-source-fn-ast - Prints out the AST of the "
                   "function.\n"
                << "-fdump-derived-fn - Prints out the source code of the "
                   "derivative.\n"
                << "-fdump-derived-fn-ast - Prints out the AST of the "
                   "derivative.\n"
                << "-fgenerate-source-file - Produces a file containing the "
                   "derivatives.\n"
                << "-fcustom-estimation-model - allows user to send in a "
                   "shared object to use as the custom estimation model.\n"
                << "-fprint-num-diff-errors - allows users to print the "
                   "calculated numerical diff errors, this flag is overriden "
                   "by -DCLAD_NO_NUM_DIFF.\n";

            llvm::errs() << "-help - Prints out this screen.\n\n";
          } else {
            llvm::errs() << "clad: Error: invalid option " << args[i] << "\n";
            return false; // Tells clang not to create the plugin.
          }
        }
        if (m_DO.ValidateClangVersion != false) {
          if (!checkClangVersion())
            return false;
        }
        return true;
      }

      PluginASTAction::ActionType getActionType() override {
        return AddAfterMainAction;
      }
    };
  } // end namespace plugin
} // end namespace clad

#endif // CLAD_CLANG_PLUGIN
