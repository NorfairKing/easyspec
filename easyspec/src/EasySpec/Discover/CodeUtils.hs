{-# LANGUAGE FlexibleContexts #-}

module EasySpec.Discover.CodeUtils where

import Import hiding (Alt)

import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.Syntax

{-# ANN module "HLint: ignore Use const" #-}

{-# ANN module "HLint: ignore Avoid lambda" #-}

{-# ANN module "HLint: ignore Collapse lambdas" #-}

getTyVars :: Type t -> [Name t]
getTyVars =
    foldType
        (\_ _ _ -> id)
        (\_ -> (++))
        (\_ _ -> concat)
        (\_ -> id)
        (\_ -> id)
        (\_ -> (++))
        (\_ -> (: []))
        (\_ _ -> [])
        (\_ -> id)
        (\_ v1 _ v2 -> v1 ++ v2)
        (\_ vs _ -> vs)
        (\_ _ -> [])
        (\_ -> (++))
        (\_ _ -> [])
        (\_ _ _ -> id)
        (\_ _ -> [])
        (\_ _ _ -> [])

foldType
    :: (l -> Maybe [TyVarBind l] -> Maybe (Context l) -> b -> b)
    -> (l -> b -> b -> b)
    -> (l -> Boxed -> [b] -> b)
    -> (l -> b -> b)
    -> (l -> b -> b)
    -> (l -> b -> b -> b)
    -> (l -> Name l -> b)
    -> (l -> QName l -> b)
    -> (l -> b -> b)
    -> (l -> b -> QName l -> b -> b)
    -> (l -> b -> Kind l -> b)
    -> (l -> Promoted l -> b)
    -> (l -> b -> b -> b)
    -> (l -> Splice l -> b)
    -> (l -> BangType l -> Unpackedness l -> b -> b)
    -> (l -> Maybe (Name l) -> b)
    -> (l -> String -> String -> b)
    -> Type l
    -> b
foldType ffa ff ft fl fpa fa fv fc fp fi fk fpr fe fspl fbng fwc fqq = go
  where
    go (TyForall l mtvbs btc t) = ffa l mtvbs btc (go t)
    go (TyFun l t1 t2) = ff l (go t1) (go t2)
    go (TyTuple l b ts) = ft l b (map go ts)
    go (TyList l lt) = fl l (go lt)
    go (TyParArray l lt) = fpa l (go lt)
    go (TyApp l t1 t2) = fa l (go t1) (go t2)
    go (TyVar l n) = fv l n
    go (TyCon l qn) = fc l qn
    go (TyParen l t) = fp l (go t)
    go (TyInfix l t1 qn t2) = fi l (go t1) qn (go t2)
    go (TyKind l t k) = fk l (go t) k
    go (TyPromoted l p) = fpr l p
    go (TyEquals l t1 t2) = fe l (go t1) (go t2)
    go (TySplice l spl) = fspl l spl
    go (TyBang l bt up t) = fbng l bt up (go t)
    go (TyWildCard l mn) = fwc l mn
    go (TyQuasiQuote l s1 s2) = fqq l s1 s2

mentions
    :: Eq l
    => Name l -> Exp l -> Bool
mentions n =
    foldExp
        (\_ qn -> q qn)
        (\_ _ -> False)
        (\_ _ -> False)
        (\_ qn -> q qn)
        (\_ _ -> False)
        (\_ b1 _ b2 -> b1 || b2)
        (\_ b1 b2 -> b1 || b2)
        (\_ b -> b)
        (\_ _ b -> b)
        (\_ _ b -> b)
        (\_ b1 b2 b3 -> b1 || b2 || b3)
        (\_ _ -> False)
        (\_ b _ -> b)
        (\_ _ -> False)
        (\_ _ -> False)
        (\_ _ bs -> or bs)
        (\_ _ mbs -> or $ catMaybes mbs)
        (\_ bs -> or bs)
        (\_ bs -> or bs)
        (\_ b -> b)
        (\_ b _ -> b)
        (\_ _ b -> b)
        (\_ qn _ -> q qn)
        (\_ b _ -> b)
        (\_ b -> b)
        (\_ b1 b2 -> b1 || b2)
        (\_ b1 b2 -> b1 || b2)
        (\_ b1 b2 b3 -> b1 || b2 || b3)
        (\_ b1 b2 -> b1 || b2)
        (\_ b1 b2 b3 -> b1 || b2 || b3)
        (\_ b _ -> b)
        (\_ b _ -> b)
        (\_ b _ -> b)
        (\_ b _ -> b)
        (\_ qn -> q qn)
        (\_ qn -> q qn)
        (\_ _ -> False)
        (\_ _ -> False)
        (\_ _ _ -> False)
        (\_ _ -> False)
        (\_ _ _ mb bs -> mb == Just True || or bs)
        (\_ _ _ mb -> mb == Just True)
        (\_ _ -> False)
        (\_ b -> b)
        (\_ bs -> or bs)
        (\_ _ b -> b)
        (\_ _ b -> b)
        (\_ _ _ _ b -> b)
        (\_ _ b -> b)
        (\_ b1 b2 -> b1 || b2)
        (\_ b1 b2 -> b1 || b2)
        (\_ b1 b2 -> b1 || b2)
        (\_ b1 b2 -> b1 || b2)
        (\_ _ -> False)
        (\_ -> False)
  where
    q (UnQual _ n') = n == n'
    q (Qual _ _ n') = n == n'
    q (Special _ _) = False

foldExp
    :: (l -> QName l -> b)
    -> (l -> String -> b)
    -> (l -> IPName l -> b)
    -> (l -> QName l -> b)
    -> (l -> Literal l -> b)
    -> (l -> b -> QOp l -> b -> b)
    -> (l -> b -> b -> b)
    -> (l -> b -> b)
    -> (l -> [Pat l] -> b -> b)
    -> (l -> Binds l -> b -> b)
    -> (l -> b -> b -> b -> b)
    -> (l -> [GuardedRhs l] -> b)
    -> (l -> b -> [Alt l] -> b)
    -> (l -> [Stmt l] -> b)
    -> (l -> [Stmt l] -> b)
    -> (l -> Boxed -> [b] -> b)
    -> (l -> Boxed -> [Maybe b] -> b)
    -> (l -> [b] -> b)
    -> (l -> [b] -> b)
    -> (l -> b -> b)
    -> (l -> b -> QOp l -> b)
    -> (l -> QOp l -> b -> b)
    -> (l -> QName l -> [FieldUpdate l] -> b)
    -> (l -> b -> [FieldUpdate l] -> b)
    -> (l -> b -> b)
    -> (l -> b -> b -> b)
    -> (l -> b -> b -> b)
    -> (l -> b -> b -> b -> b)
    -> (l -> b -> b -> b)
    -> (l -> b -> b -> b -> b)
    -> (l -> b -> [QualStmt l] -> b)
    -> (l -> b -> [[QualStmt l]] -> b)
    -> (l -> b -> [[QualStmt l]] -> b)
    -> (l -> b -> Type l -> b)
    -> (l -> QName l -> b)
    -> (l -> QName l -> b)
    -> (l -> Bracket l -> b)
    -> (l -> Splice l -> b)
    -> (l -> String -> String -> b)
    -> (l -> Type l -> b)
    -> (l -> XName l -> [XAttr l] -> Maybe b -> [b] -> b)
    -> (l -> XName l -> [XAttr l] -> Maybe b -> b)
    -> (l -> String -> b)
    -> (l -> b -> b)
    -> (l -> [b] -> b)
    -> (l -> String -> b -> b)
    -> (l -> String -> b -> b)
    -> (l -> String -> (Int, Int) -> (Int, Int) -> b -> b)
    -> (l -> Pat l -> b -> b)
    -> (l -> b -> b -> b)
    -> (l -> b -> b -> b)
    -> (l -> b -> b -> b)
    -> (l -> b -> b -> b)
    -> (l -> [Alt l] -> b)
    -> (l -> b)
    -> Exp l
    -> b
foldExp ff1 ff2 ff3 ff4 ff5 ff6 ff7 ff8 ff9 ff10 ff11 ff12 ff13 ff14 ff15 ff16 ff17 ff18 ff19 ff20 ff21 ff22 ff23 ff24 ff25 ff26 ff27 ff28 ff29 ff30 ff31 ff32 ff33 ff34 ff35 ff36 ff37 ff38 ff39 ff40 ff41 ff42 ff43 ff44 ff45 ff46 ff47 ff48 ff49 ff50 ff51 ff52 ff53 ff54 ff55 =
    go
  where
    go (Var l qn) = ff1 l qn
    go (OverloadedLabel l s) = ff2 l s
    go (IPVar l ipn) = ff3 l ipn
    go (Con l qn) = ff4 l qn
    go (Lit l lit) = ff5 l lit
    go (InfixApp l b1 qop b2) = ff6 l (go b1) qop (go b2)
    go (App l b1 b2) = ff7 l (go b1) (go b2)
    go (NegApp l b) = ff8 l (go b)
    go (Lambda l pats b) = ff9 l pats (go b)
    go (Let l bnds b) = ff10 l bnds (go b)
    go (If l b1 b2 b3) = ff11 l (go b1) (go b2) (go b3)
    go (MultiIf l grhs) = ff12 l grhs
    go (Case l b as) = ff13 l (go b) as
    go (Do l stmts) = ff14 l stmts
    go (MDo l stmts) = ff15 l stmts
    go (Tuple l bd bs) = ff16 l bd (map go bs)
    go (TupleSection l bxd mbs) = ff17 l bxd (map (fmap go) mbs)
    go (List l bs) = ff18 l (map go bs)
    go (ParArray l bs) = ff19 l (map go bs)
    go (Paren l b) = ff20 l (go b)
    go (LeftSection l b qop) = ff21 l (go b) qop
    go (RightSection l qop b) = ff22 l qop (go b)
    go (RecConstr l qn fos) = ff23 l qn fos
    go (RecUpdate l b fos) = ff24 l (go b) fos
    go (EnumFrom l b) = ff25 l (go b)
    go (EnumFromTo l b1 b2) = ff26 l (go b1) (go b2)
    go (EnumFromThen l b1 b2) = ff27 l (go b1) (go b2)
    go (EnumFromThenTo l b1 b2 b3) = ff28 l (go b1) (go b2) (go b3)
    go (ParArrayFromTo l b1 b2) = ff29 l (go b1) (go b2)
    go (ParArrayFromThenTo l b1 b2 b3) = ff30 l (go b1) (go b2) (go b3)
    go (ListComp l b qstms) = ff31 l (go b) qstms
    go (ParComp l b qstmss) = ff32 l (go b) qstmss
    go (ParArrayComp l b qstmss) = ff33 l (go b) qstmss
    go (ExpTypeSig l b ts) = ff34 l (go b) ts
    go (VarQuote l qn) = ff35 l qn
    go (TypQuote l qn) = ff36 l qn
    go (BracketExp l braq) = ff37 l braq
    go (SpliceExp l splice) = ff38 l splice
    go (QuasiQuote l s s2) = ff39 l s s2
    go (TypeApp l t) = ff40 l t
    go (XTag l xnam xas mb bs) = ff41 l xnam xas (go <$> mb) (map go bs)
    go (XETag l xnam xas mb) = ff42 l xnam xas (go <$> mb)
    go (XPcdata l s) = ff43 l s
    go (XExpTag l b) = ff44 l (go b)
    go (XChildTag l bs) = ff45 l (map go bs)
    go (CorePragma l s b) = ff46 l s (go b)
    go (SCCPragma l s b) = ff47 l s (go b)
    go (GenPragma l s t1 t2 b) = ff48 l s t1 t2 (go b)
    go (Proc l pat b) = ff49 l pat (go b)
    go (LeftArrApp l b1 b2) = ff50 l (go b1) (go b2)
    go (RightArrApp l b1 b2) = ff51 l (go b1) (go b2)
    go (LeftArrHighApp l b1 b2) = ff52 l (go b1) (go b2)
    go (RightArrHighApp l b1 b2) = ff53 l (go b1) (go b2)
    go (LCase l as) = ff54 l as
    go (ExprHole l) = ff55 l

prettyPrintOneLine
    :: Pretty a
    => a -> String
prettyPrintOneLine =
    prettyPrintStyleMode (style {mode = OneLineMode}) defaultMode
