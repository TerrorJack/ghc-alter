module Language.Haskell.GHC.Alter.Caliburn
  ( initCompiler
  ) where

import BasicTypes
import HsSyn
import HscTypes
import Language.Haskell.GHC.Alter.Compiler
import Language.Haskell.GHC.Alter.LoneWolf ()
import RdrName
import SrcLoc
import TcEvidence

stripFFIDecl :: HsDecl RdrName -> [HsDecl RdrName]
stripFFIDecl (ForD ForeignImport { fd_name = loc_name
                                 , fd_sig_ty = HsIB {hsib_body = loc_ty}
                                 }) =
  [ SigD $
    TypeSig
      [loc_name]
      HsWC
      { hswc_wcs = PlaceHolder
      , hswc_body =
          HsIB
          { hsib_vars = PlaceHolder
          , hsib_body = loc_ty
          , hsib_closed = PlaceHolder
          }
      }
  , ValD
      FunBind
      { fun_id = loc_name
      , fun_matches =
          MG
          { mg_alts =
              L
                noSrcSpan
                [ L
                    noSrcSpan
                    Match
                    { m_ctxt =
                        FunRhs
                        { mc_fun = loc_name
                        , mc_fixity = Prefix
                        , mc_strictness = NoSrcStrict
                        }
                    , m_pats = []
                    , m_type = Nothing
                    , m_grhss =
                        GRHSs
                        { grhssGRHSs =
                            [ L noSrcSpan $
                              GRHS [] $ L noSrcSpan $ HsVar loc_name
                            ]
                        , grhssLocalBinds = L noSrcSpan EmptyLocalBinds
                        }
                    }
                ]
          , mg_arg_tys = []
          , mg_res_ty = PlaceHolder
          , mg_origin = FromSource
          }
      , fun_co_fn = WpHole
      , bind_fvs = PlaceHolder
      , fun_tick = []
      }
  ]
stripFFIDecl (ForD ForeignExport {}) = []
stripFFIDecl decl = [decl]

stripFFIModule :: HsParsedModule -> HsParsedModule
stripFFIModule hpm@HsParsedModule {hpm_module = L src_loc hm@HsModule {hsmodDecls = decls}} =
  hpm
  { hpm_module =
      L
        src_loc
        hm
        { hsmodDecls =
            [ L orig_src_loc new_decl
            | L orig_src_loc orig_decl <- decls
            , new_decl <- stripFFIDecl orig_decl
            ]
        }
  }

initCompiler :: IO Compiler
initCompiler = pure $ defaultCompiler {patch = \_ -> pure . stripFFIModule}
