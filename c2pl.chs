-- Code adapted from example_usage_of_language.c_(from_2009).c by user786653
-- See http://stackoverflow.com/a/6337802/3179747
-- and http://pastebin.com/7ihKY0KV
import System.IO
import System.IO.Unsafe
import System.Environment

import Data.List
import Data.Maybe

import Control.Monad
import Control.Arrow

import Language.C
import Language.C.Data
import Language.C.Data.Ident
import Language.C.Analysis
import Language.C.Analysis.Export
import Language.C.Analysis.SemRep
import Language.C.System.GCC

--Prolog header included for macros
#include <gprolog/gprolog.h>

--Debug
import Debug.Trace

type State = [CExternalDeclaration NodeInfo]

main :: IO ()
main = do
    let usage = error "give file to parse"
    (opts,c_file) <- liftM (init &&& last) getArgs

    let compiler = newGCC "gcc"
    ast <- parseCFile compiler Nothing opts c_file >>= checkResult "[parsing]"

    putStrLn "#include <gprolog/gprolog.h>"
    case (runTrav newState (withExtDeclHandler (analyseAST ast) handler)) of
     Left errs -> do putStrLn "errors"
     Right (decls, state) -> putStrLn . asS $ CTranslUnit (userState state) udn

    where
    checkResult :: (Show a) => String -> (Either a b) -> IO b
    checkResult label = either (error . (label++) . show) return

newState :: State
newState = []

asS :: (Pretty a) => a -> String
asS = show . pretty 

handler :: DeclEvent -> Trav State ()
handler (DeclEvent (FunctionDef fd)) =
    do error "FunctionDef not implemented"
       return ()
handler (DeclEvent (Declaration d)) = let fun = generateFunction $ getVarDecl d in 
    modifyUserState (\s -> fun : s)
handler _ = return ()


--CDeclarationSpecifier definitations for common C types
intTypeSpec :: CDeclarationSpecifier NodeInfo
intTypeSpec = CTypeSpec $ CIntType udn 

longTypeSpec :: CDeclarationSpecifier NodeInfo
longTypeSpec = CTypeSpec $ CLongType udn

plTermTypeSpec :: CDeclarationSpecifier NodeInfo
plTermTypeSpec = CTypeSpec $ CTypeDef (internalIdent "PlTerm") udn 

boolTypeSpec :: CDeclarationSpecifier NodeInfo
boolTypeSpec = CTypeSpec $ CTypeDef (internalIdent "Bool") udn

--CExpression definitaions for common C constant expressions
intConst :: Integer -> CExpression NodeInfo
intConst n = CConst $ CIntConst (cInteger n) udn

strConst :: String -> CExpression NodeInfo
strConst str = CConst $ CStrConst (cString str) udn

--CDerivedDeclarator generating functions
arrayDeclr :: Integer -> CDerivedDeclarator NodeInfo
arrayDeclr n = CArrDeclr [] (CArrSize False (intConst n)) udn

--Generate CExpression to call a function
callFunction :: String -> [CExpression NodeInfo] -> CExpression NodeInfo
callFunction f argv = CCall (CVar (internalIdent f) udn) argv udn 

--A miracle of pattern matching
identOfCDecl (CDecl _ ((Just (CDeclr (Just id) _ _ _ _),_,_):_) _) = Just id
identOfCDecl _ = Nothing

mkVarOrIntAST (id, i) = CBlockStmt $ CIf 
                       (CUnary CIndOp var udn)
                       (CExpr (Just $
                           CAssign 
                               CAssignOp 
                               (CIndex (CVar (internalIdent "arg") udn) (intConst i) udn)
                               (callFunction "Mk_Integer" [CUnary CIndOp (CUnary CIndOp var udn) udn])
                               udn)
                           udn)
                       (Just $ CExpr (Just $
                           CAssign
                               CAssignOp
                               (CIndex (CVar (internalIdent "arg") udn) (intConst i) udn)
                               (callFunction "Mk_Variable" [])
                               udn)
                           udn)
                       udn
                       where var = CVar id udn

rdIfNullAST (id, i) = CBlockStmt $ CIf
                          (CUnary CNegOp (CUnary CIndOp var udn) udn)
                          (CCompound 
                              []
                              [
                                  CBlockStmt $ CExpr (Just $
                                      CAssign
                                          CAssignOp
                                          (CUnary CIndOp var udn)
                                          (callFunction "malloc" [CSizeofType (CDecl [longTypeSpec] [] udn) udn])
                                          udn)
                                      udn,
                                  CBlockStmt $ CExpr (Just $
                                      CAssign
                                          CAssignOp
                                          (CUnary CIndOp (CUnary CIndOp var udn) udn)
                                          (callFunction "Rd_Integer_Check" [CIndex (CVar (internalIdent "arg") udn) (intConst i) udn])
                                          udn)
                                      udn
                              ]
                              udn)
                          Nothing
                          udn
                          where var = CVar id udn

--Because typing undefNode is too much work
udn = undefNode

generateFunction :: VarDecl -> CExternalDeclaration NodeInfo
generateFunction (VarDecl varName declAttr t) = CFDefExt $ CFunDef
    spec
    (CDeclr (Just funName) args Nothing [] udn)
    ([] :: [CDeclaration NodeInfo]) -- optional delcaration list for old style fundef (not used)
    funBody
    udn
    where (spec,args) = exportType t                       --Args and type spec from protype
          funName     = identOfVarName varName             --Function name is same as the prototype
          (argc,argIds) = case args of
                          ((CFunDeclr (Right (argv,_)) _ _):_) -> (genericLength argv,zipWith (\d n -> (fromMaybe (internalIdent ("__"++asS funName++(show n))) (identOfCDecl d),n)) argv [0..])
          funBody     = CCompound                          --Function body must be genetaed
                            []                             --Local labels (will not be used)
                            ([                              --Function code
                                CBlockDecl $ CDecl         --    PlTerm arg[1];
                                    [plTermTypeSpec]
                                    [(
                                        Just $ CDeclr (Just $ internalIdent "arg") [arrayDeclr argc] Nothing [] udn,
                                        Nothing,
                                        Nothing
                                    )] 
                                    udn,
                                CBlockDecl $ CDecl         --    int func = Find_Atom(FUNCTION_NAME);
                                    [intTypeSpec]          
                                    [(
                                        Just $ CDeclr (Just $ internalIdent "func") [] Nothing [] udn,
                                        Just $ CInitExpr (callFunction "Find_Atom" [strConst $ identToString funName] ) udn,
                                        Nothing
                                    )]
                                    udn,
                                CBlockStmt $ CExpr         --    PL_Query_Begin(1)
                                    (Just $ callFunction "Pl_Query_Begin" [intConst {#const TRUE#}] )
                                    udn 
                            ] ++ (map mkVarOrIntAST argIds) ++ [
                                CBlockDecl $ CDecl
                                    [boolTypeSpec]
                                    [(
                                        Just $ CDeclr (Just $ internalIdent "res") [] Nothing [] udn,
                                        Just $ CInitExpr (callFunction "Pl_Query_Call" [CVar (internalIdent "func") udn,intConst 1,CVar (internalIdent "arg") udn]) udn,
                                        Nothing
                                    )]
                                    udn,
                                CBlockStmt $ CIf  
                                    (CBinary CEqOp (CVar (internalIdent "res") udn) (intConst {#const PL_SUCCESS#}) udn)
                                    (CCompound [] (map rdIfNullAST argIds) udn)
                                    Nothing
                                    udn,
                                CBlockStmt $ CExpr 
                                    (Just $ callFunction "Pl_Query_End" [intConst {#const PL_RECOVER#}])
                                    udn,
                                CBlockStmt $ CReturn (Just $ CVar (internalIdent "res") udn) udn
                            ])
                            udn  
