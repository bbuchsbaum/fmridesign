options(mc.cores=2)

library(testthat)
library(assertthat)
library(fmrihrf)

# Try to load face design from fmrireg if available; otherwise, synthesize a minimal dataset
facedes <- NULL
try({
  suppressPackageStartupMessages(library(fmrireg))
  facedes <- try(read.table(system.file("extdata", "face_design.txt", package = "fmrireg"), header=TRUE), silent = TRUE)
  if (inherits(facedes, "try-error")) facedes <- NULL
}, silent = TRUE)

if (is.null(facedes)) {
  set.seed(1)
  n <- 10
  facedes <- data.frame(
    onset = seq(0, by = 4, length.out = n),
    rep_num = sample(c(-1, 1, 2, 3, 4), n, replace = TRUE),
    run = rep(1, n)
  )
}


test_that("a 2-by-2 Fcontrast", {
  F_a <- factor(rep(letters[1:2], 8))
  F_b <- factor(rep(c("V1", "V2"), each=8))
  onsets <- seq(1, length(F_a))
  blockids <- rep(1, length(onsets))
  
  et <- event_term(list(Fa=F_a, Fb=F_b), onsets, blockids)
  expect_true(!is.null(Fcontrasts(et)))
})

test_that("a 2-by-3 Fcontrast", {
  F_a <- factor(rep(letters[1:2], 9))
  F_b <- factor(rep(c("V1", "V2", "V3"), each=6))
 
  onsets <- seq(1, length(F_a))
  blockids <- rep(1, length(onsets))
  
  et <- event_term(list(Fa=F_a, Fb=F_b), onsets, blockids)
  expect_true(!is.null(Fcontrasts(et)))
})


test_that("a 3-by-2 Fcontrast", {
  F_a <- factor(rep(letters[1:2], 8))
  F_b <- factor(rep(c("V1", "V2"), each=8))
  F_c <- factor(rep(c("B3", "B3", "B4", "B4"), 4))
  onsets <- seq(1, length(F_a))
  blockids <- rep(1, length(onsets))
  
  et <- event_term(list(Fa=F_a, Fb=F_b, Fc=F_c), onsets, blockids)
  expect_true(!is.null(Fcontrasts(et)))
})

test_that("a 3-by-3 Fcontrast", {
  F_a <- factor(sample(letters[1:3], 200, replace=TRUE))
  F_b <- factor(sample(c("V1", "V2", "V3"), 200, replace=TRUE))
  F_c <- factor(sample(c("B3", "B3", "B4", "B4", "B5", "B5"),200, replace=TRUE))
  onsets <- seq(1, length(F_a))
  blockids <- rep(1, length(onsets))
  
  et <- event_term(list(Fa=F_a, Fb=F_b, Fc=F_c), onsets, blockids)
  expect_true(!is.null(Fcontrasts(et)))
})




test_that("can build a simple contrast from a convolved term", {
  facedes$repnum <- factor(facedes$rep_num)
  sframe <- fmrihrf::sampling_frame(blocklens=rep(436/2,max(facedes$run)), TR=2)
  espec <- event_model(onset ~  hrf(repnum), data=facedes, block=~run, sampling_frame=sframe)
  con <- pair_contrast(~ repnum==-1, ~ repnum==1, name="A_B")
  
  expect_equal(as.vector(contrast_weights(con, terms(espec)[[1]])$weights), c(1,-1,0,0,0))
})

test_that("can build a simple contrast from a convolved term and convert to glt", {
  skip_if_not(exists("to_glt", mode = "function"), "AFNI export (to_glt) moved out of fmridesign")
  facedes$repnum <- factor(facedes$rep_num)
  sframe <- fmrihrf::sampling_frame(blocklens=rep(436/2,max(facedes$run)), TR=2)
  espec <- event_model(onset ~  hrf(repnum), data=facedes, block=~run, sampling_frame=sframe)
  con <- pair_contrast(~ repnum==-1, ~ repnum==1, name="A_B")
  
  conw <- contrast_weights(con, terms(espec)[[1]])
  glt <- to_glt(conw)
  expect_true(!is.null(glt))
})

test_that("can build a contrast versus the intercept from a convolved term", {
  facedes$repnum <- factor(facedes$rep_num)
  sframe <- fmrihrf::sampling_frame(blocklens=rep(436/2,max(facedes$run)), TR=2)
  espec <- event_model(onset ~  hrf(repnum), data=facedes, block=~run, sampling_frame=sframe)
  
  con <- unit_contrast(~ repnum, name="A")

  term <- terms(espec)[[1]]
  expect_equal(as.vector(contrast_weights(con, term)$weights), rep(.2,5))
  
})

test_that("can construct a simple pair_contrast", {
  facedes$repnum <- factor(facedes$rep_num)
  sframe <- fmrihrf::sampling_frame(blocklens=rep(436/2,max(facedes$run)), TR=2)
  espec <- event_model(onset ~  hrf(repnum), data=facedes, block=~run, sampling_frame=sframe)
  
  pc <- pair_contrast(~ repnum == 1, ~ repnum ==2, name="B-A")
  cw <- contrast_weights(pc, terms(espec)[[1]])
  expect_equal(as.vector(cw$weights), c(0,1,-1,0,0))
  
})

test_that("can build a linear contrast from repnum and value_map", {
   facedes$repnum <- factor(facedes$rep_num)
   sframe <- fmrihrf::sampling_frame(blocklens=rep(436/2,max(facedes$run)), TR=2)
   espec <- event_model(onset ~  hrf(repnum), data=facedes, block=~run, sampling_frame=sframe)
  
   con <- poly_contrast(~ repnum, degree=1, value_map=list("-1"=0, "1"=1, "2"=2, "3"=3, "4"=4), name="linear_repnum")
   term1 <- terms(espec)[[1]]
   cw <- contrast_weights(con, term1)
   expect_equal(as.vector(contrast_weights(con, term1)$weights), as.vector(poly(c(0,1,2,3,4))))
})

test_that("can build a set of pairwise contrasts", {
  facedes$repnum <- factor(facedes$rep_num)
  sframe <- fmrihrf::sampling_frame(blocklens=rep(436/2,max(facedes$run)), TR=2)
  espec <- event_model(onset ~  hrf(repnum), data=facedes, block=~run, sampling_frame=sframe)
  levs <- levels(facedes$repnum)
  cset <- pairwise_contrasts(levs, facname = "repnum")
  expect_equal(length(cset), ncol(combn(length(levs),2)))

})

test_that("can build a one_against_all contrast set", {
  facedes$repnum <- factor(facedes$rep_num)
  sframe <- fmrihrf::sampling_frame(blocklens=rep(436/2,max(facedes$run)), TR=2)
  espec <- event_model(onset ~  hrf(repnum), data=facedes, block=~run, sampling_frame=sframe)
  levs <- levels(facedes$repnum)
  cset <- one_against_all_contrast(levs, "repnum")
  
  
  expect_equal(length(cset),length(levels(facedes$repnum)))
  
  wtls <- lapply(cset, function(con) {
    contrast_weights(con, terms(espec)[[1]])
  })
  expect_true(!is.null(wtls))
  
})

test_that("can subtract two pairwise contrasts to form an interaction contrast", {
  simple_des <- expand.grid(category=c("face", "scene"), attention=c("attend", "ignored"), replication=c(1,2))
  simple_des$onset <- seq(1,100, length.out=nrow(simple_des))
  simple_des$run <- rep(1,nrow(simple_des))
  sframe <- fmrihrf::sampling_frame(blocklens=100, TR=2)
  espec <- event_model(onset ~  hrf(category, attention), data=simple_des, block=~run, sampling_frame=sframe)
  con1 <- pair_contrast(~ category=="face", ~ category == "scene", name="face_scene#attend", where=~ attention == "attend")
  con2 <- pair_contrast(~ category=="face", ~ category == "scene", name="face_scene#ignored", where=~ attention == "ignored")
  con3 <- con1 - con2
  expect_true(!is.null(con3))
})


test_that("can contrast two parametric regressors crossed with a factor", {
  simple_des <- expand.grid(category=c("face", "scene"), attention=c("attend", "ignored"), replication=c(1,2))
  simple_des$onset <- seq(1,100, length.out=nrow(simple_des))
  simple_des$run <- rep(1,nrow(simple_des))
  simple_des$RT <- rnorm(nrow(simple_des))
  
  sframe <- fmrihrf::sampling_frame(blocklens=100, TR=2)
  espec <- event_model(onset ~  hrf(category), data=simple_des, block=~run, sampling_frame=sframe)
  con <- pair_contrast(~ category == "face", ~ category == "scene", name="face_vs_scene")
  cwts <- contrast_weights(con, terms(espec)[[1]])
  expect_true(!is.null(cwts))
})

test_that("can contrast two parametric regressors wrapped in Ident for additive regressors", {
  simple_des <- expand.grid(category=c("face", "scene"), attention=c("attend", "ignored"), replication=c(1,2))
  simple_des$onset <- seq(1,100, length.out=nrow(simple_des))
  simple_des$run <- rep(1,nrow(simple_des))
  simple_des$RT1 <- rnorm(nrow(simple_des))
  simple_des$RT2 <- rnorm(nrow(simple_des))
  
  sframe <- fmrihrf::sampling_frame(blocklens=100, TR=2)
  espec <- event_model(onset ~  hrf(Ident(RT1,RT2)), data=simple_des, block=~run, sampling_frame=sframe)
  
  # --- Determine actual column names for safety ---
  dm_colnames <- colnames(design_matrix(espec))
  term_of_interest <- terms(espec)[[1]] 
  term_condition_tags <- conditions(term_of_interest, expand_basis=FALSE) 
  term_tag_assigned <- attr(term_of_interest, "term_tag")
  
  # For Ident-only terms, term_tag might be NULL according to new naming scheme
  # In that case, column names should be just the variable names (RT1, RT2)
  if (is.null(term_tag_assigned)) {
    # Ident-only case: column names should be just the variable names
    pattern_A_col <- "^RT1$"
    pattern_B_col <- "^RT2$"
  } else {
    # Regular case with term_tag prefix
    if (length(term_condition_tags) < 2) {
      stop("Could not reliably determine column names for Ident(RT1,RT2) term in test.")
    }
    pattern_A_col <- paste0("^", term_tag_assigned, "_", term_condition_tags[1], "$")
    pattern_B_col <- paste0("^", term_tag_assigned, "_", term_condition_tags[2], "$")
  }
  
  # Ensure these patterns actually match columns in the design_matrix
  expect_true(any(grepl(pattern_A_col, dm_colnames)), 
              info = paste("Pattern A:", pattern_A_col, "did not match any of actual colnames:", paste(dm_colnames, collapse=", ")))
  expect_true(any(grepl(pattern_B_col, dm_colnames)),
              info = paste("Pattern B:", pattern_B_col, "did not match any of actual colnames:", paste(dm_colnames, collapse=", ")))

  con <- column_contrast(pattern_A = pattern_A_col, 
                         pattern_B = pattern_B_col, 
                         name="RT1_vs_RT2_cols")
                         
  cwts <- contrast_weights(con, term_of_interest)
  expect_true(!is.null(cwts))
  
  # More specific checks for the weights vector
  weights_vec <- as.vector(cwts$weights)
  col_A_idx <- grep(pattern_A_col, dm_colnames)
  col_B_idx <- grep(pattern_B_col, dm_colnames)
  
  # Check that only one column matches each pattern
  expect_equal(length(col_A_idx), 1, info = "Pattern A should match exactly one column")
  expect_equal(length(col_B_idx), 1, info = "Pattern B should match exactly one column")
  
  if (length(col_A_idx) == 1 && length(col_B_idx) == 1) {
      expect_equal(weights_vec[col_A_idx], 1)
      expect_equal(weights_vec[col_B_idx], -1)
      # Ensure all other weights are zero
      other_indices <- setdiff(seq_along(weights_vec), c(col_A_idx, col_B_idx))
      if (length(other_indices) > 0) {
        expect_true(all(weights_vec[other_indices] == 0))
      }
      expect_equal(sum(weights_vec), 0) # Overall sum should be zero
  }

})

test_that("can contrast two basis functions from a custom multi-phase hrf", {
  # Create a proper factor with multiple levels to avoid the contrasts error
  simple_des <- expand.grid(trial_type=c("encoding", "retrieval"))
  simple_des <- simple_des[rep(1:nrow(simple_des), length.out=15), , drop=FALSE]
  
  simple_des$onset <- seq(1,300, length.out=nrow(simple_des))
  simple_des$run <- rep(1,nrow(simple_des))
  
  hrf_encode <- suppressWarnings(fmrihrf::gen_hrf(fmrihrf::hrf_spmg1, normalize=TRUE))
  hrf_delay <- suppressWarnings(fmrihrf::gen_hrf(fmrihrf::hrf_spmg1, lag=3, width=8, normalize=TRUE))
  hrf_probe <- suppressWarnings(fmrihrf::gen_hrf(fmrihrf::hrf_spmg1, lag=11, width=3, normalize=TRUE))
  hrf_trial <<- suppressWarnings(fmrihrf::hrf_set(hrf_encode, hrf_delay, hrf_probe))
  
  sframe <- fmrihrf::sampling_frame(blocklens=250, TR=2)
  espec <- suppressWarnings(event_model(onset ~  hrf(trial_type, basis=hrf_trial), data=simple_des, block=~run, sampling_frame=sframe))
  
  # Updated to use new HRF basis suffix naming scheme: _b01, _b02, etc.
  # Use column_contrast to target specific basis functions
  con <- column_contrast(pattern_A = "_b01$", pattern_B = "_b02$", name="basis1_vs_basis2")
  cwts <- contrast_weights(con, terms(espec)[[1]])
  expect_true(!is.null(cwts))
})

test_that("can form a simple formula contrast", {
  simple_des <- expand.grid(category=c("face", "scene"), attention=c("attend", "ignored"), replication=c(1,2))
  simple_des$onset <- seq(1,100, length.out=nrow(simple_des))
  simple_des$run <- rep(1,nrow(simple_des))
  sframe <- fmrihrf::sampling_frame(blocklens=100, TR=2)
  espec <- event_model(onset ~  hrf(category, attention), data=simple_des, block=~run, sampling_frame=sframe)
  
  # Use the new naming scheme: term_tag_condition_tag format
  # The term_tag should be "category_attention" and condition_tags should be "category.face_attention.attend" etc.
  # But for formula contrasts, we still use shortnames which are the old format for backward compatibility
  con1 <- contrast(~ (`face:attend` - `face:ignored`) - (`scene:attend` - `scene:ignored`), name="face_scene")
  cwts <- contrast_weights(con1, terms(espec)[[1]])
  
  # Now the contrast should work correctly with proper weights
  # Order is: face:attend, scene:attend, face:ignored, scene:ignored
  # Formula: (face:attend - face:ignored) - (scene:attend - scene:ignored)
  # Expected: face:attend=1, face:ignored=-1, scene:attend=-1, scene:ignored=1
  expect_equal(as.vector(cwts$weights[,1]), c(1, -1, -1, 1))
  
})

test_that("can form formula contrast with 3 terms", {
  simple_des <- expand.grid(match=c("match", "nonmatch"), condition=c("NOVEL", "REPEAT"), correct=c("correct","incorrect"))
  simple_des$onset <- seq(1,100, length.out=nrow(simple_des))
  simple_des$run <- rep(1,nrow(simple_des))
  sframe <- fmrihrf::sampling_frame(blocklens=100, TR=2)
  espec <- event_model(onset ~  hrf(match, condition, correct), data=simple_des, block=~run, sampling_frame=sframe)
  
  # Use the old shortnames format for formula contrasts (backward compatibility)
  con1 <- contrast(
    ~  ((`match:NOVEL:correct` + `match:NOVEL:incorrect`) - (`nonmatch:NOVEL:correct` + `nonmatch:NOVEL:incorrect`)) -
      ((`match:REPEAT:correct` + `match:REPEAT:incorrect`) - (`nonmatch:REPEAT:correct` + `nonmatch:REPEAT:incorrect`)), name="cond_by_match")
  cwts <- contrast_weights(con1, terms(espec)[[1]])
  expect_equal(length(as.vector(cwts$weights[,1])), 8)
  
})

test_that("can form formula contrast with two factor terms and one continuous covariate", {
  simple_des <- expand.grid(match=c("match", "nonmatch"), condition=c("NOVEL", "REPEAT"), correct=c(1,2))
  simple_des$onset <- seq(1,100, length.out=nrow(simple_des))
  simple_des$run <- rep(1,nrow(simple_des))
  simple_des$correct <- as.factor(simple_des$correct)
  sframe <- fmrihrf::sampling_frame(blocklens=100, TR=2)
  espec <- event_model(onset ~  hrf(match, condition, correct), data=simple_des, block=~run, sampling_frame=sframe)
  
  # Use the old shortnames format for formula contrasts
  con1 <- contrast(
    ~  `match:NOVEL:1` - `nonmatch:NOVEL:1`, name="cond_by_match")
  cwts <- contrast_weights(con1, terms(espec)[[1]])
  
  # Check that we get the correct contrast weights
  weights_vec <- as.vector(cwts$weights[,1])
  expect_equal(length(weights_vec), 8)
  
  # The contrast should have +1 for match:NOVEL:1 and -1 for nonmatch:NOVEL:1
  # Find the positions of these conditions in shortnames
  short_names <- shortnames(terms(espec)[[1]])
  match_pos <- which(short_names == "match:NOVEL:1")
  nonmatch_pos <- which(short_names == "nonmatch:NOVEL:1")
  
  expect_equal(weights_vec[match_pos], 1)
  expect_equal(weights_vec[nonmatch_pos], -1)
  # All other weights should be 0
  other_pos <- setdiff(1:length(weights_vec), c(match_pos, nonmatch_pos))
  expect_true(all(weights_vec[other_pos] == 0))
  
})



# 
# test_that("can build a contrast versus the intercept and add to hrfspec", {
#   facedes$repnum <- factor(facedes$rep_num)
#   aux_table <- data.frame(run=rep(1:6, each=218))
#   
#   
#   conf <- contrast_formula(~ `2` - !`1`, id="repnum")
#   sframe <- fmrihrf::sampling_frame(rep(436/2,max(facedes$run)), TR=2)
#   nuisance <- matrix(rnorm(2*length(sframe$blockids)), length(sframe$blockids), 2)
#   
#   bm <- baseline_model(basis="bs", degree=3, sampling_frame=sframe, nuisance_matrix=nuisance)
#   em <- event_model(onset ~ hrf(repnum, contrasts=con), block = ~ run, data=facedes, sampling_frame=sframe)
#   mod <- fmri_model(em, bm)
#   
#   term <- construct(mspec$varspec[[1]], mspec)
#   expect_equal(as.vector(contrast_weights(con, term)), c(1,0,0,0,0))
# })
# 


test_that("pair_contrast respects where clause", {
  simple_des <- expand.grid(category=c("face", "scene"), attention=c("attend", "ignored"))
  simple_des$onset <- seq(1, 50, length.out=nrow(simple_des))
  simple_des$run <- 1
  sframe <- fmrihrf::sampling_frame(blocklens=50, TR=2)
  espec <- event_model(onset ~ hrf(category, attention), data=simple_des, block=~run, sampling_frame=sframe)

  con <- pair_contrast(~ category == "face", ~ category == "scene", name="face_vs_scene_attend", where = ~ attention == "attend")
  cw <- contrast_weights(con, terms(espec)[[1]])
  wvec <- as.vector(cw$weights)
  sn <- shortnames(terms(espec)[[1]])
  face_att <- which(sn == "face:attend")
  scene_att <- which(sn == "scene:attend")
  face_ign <- which(sn == "face:ignored")
  scene_ign <- which(sn == "scene:ignored")

  expect_equal(wvec[face_att], 1)
  expect_equal(wvec[scene_att], -1)
  expect_equal(wvec[face_ign], 0)
  expect_equal(wvec[scene_ign], 0)
})

test_that("unit_contrast allows where subsetting", {
  simple_des <- expand.grid(category=c("face", "scene"), attention=c("attend", "ignored"))
  simple_des$onset <- seq(1, 50, length.out=nrow(simple_des))
  simple_des$run <- 1
  sframe <- fmrihrf::sampling_frame(blocklens=50, TR=2)
  espec <- event_model(onset ~ hrf(category, attention), data=simple_des, block=~run, sampling_frame=sframe)

  con <- unit_contrast(~ category, name="face_only_attend", where = ~ attention == "attend" & category == "face")
  term1 <- terms(espec)[[1]]
  cw <- contrast_weights(con, term1)
  conds <- conditions(term1, drop.empty = FALSE, expand_basis = FALSE)
  expect_equal(rownames(cw$weights), conds)
  wvec <- as.vector(cw$weights)
  idx_face_att <- which(conds == "category.face_attention.attend")
  expect_equal(wvec[idx_face_att], 1)
  expect_true(all(wvec[-idx_face_att] == 0))
})

test_that("poly_contrast respects where clause", {
  des <- expand.grid(repnum=factor(1:4), grp=c("A","B"))
  des$onset <- seq_len(nrow(des))
  des$run <- 1
  sframe <- fmrihrf::sampling_frame(blocklens=50, TR=2)
  espec <- event_model(onset ~ hrf(repnum, grp), data=des, block=~run, sampling_frame=sframe)

  con <- poly_contrast(~ repnum, name="polyrep", degree=1, where=~ grp == "A")
  term1 <- terms(espec)[[1]]
  cw <- contrast_weights(con, term1)
  conds <- conditions(term1, drop.empty=FALSE, expand_basis=FALSE)
  expect_equal(rownames(cw$weights), conds)
  poly_vals <- as.vector(poly(1:4, degree=1))
  idx_A <- grep("grp.A$", conds)
  idx_B <- grep("grp.B$", conds)
  expect_equal(as.vector(cw$weights[idx_A,1]), poly_vals)
  expect_true(all(cw$weights[idx_B,1] == 0))
})

# ====================================================================
# Basis filtering tests
# ====================================================================

test_that("pair_contrast with basis filtering - single basis", {
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    condition = rep(c("A", "B", "C"), 10),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  espec <- event_model(onset ~ hrf(condition, basis = "bspline", nbasis = 5),
                      data = des, block = ~run, sampling_frame = sframe)

  # Test filtering to basis 1 only
  con <- pair_contrast(~ condition == "A", ~ condition == "B", basis = 1, name = "A_vs_B_b1")
  term1 <- terms(espec)[[1]]
  cw <- contrast_weights(con, term1)

  # Should only have basis 1 conditions
  expect_true(all(grepl("_b01$", cw$condnames)))
  expect_equal(length(cw$condnames), 3)  # A_b01, B_b01, C_b01

  # Check that weights are non-zero only for A_b01 and B_b01
  wvec <- as.vector(cw$weights)
  nonzero_idx <- which(wvec != 0)
  expect_equal(length(nonzero_idx), 2)
  expect_equal(wvec[grepl("condition\\.A_b01", rownames(cw$weights))], 1)
  expect_equal(wvec[grepl("condition\\.B_b01", rownames(cw$weights))], -1)
})

test_that("pair_contrast with basis filtering - multiple basis", {
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    condition = rep(c("Face", "Scene"), 15),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  espec <- event_model(onset ~ hrf(condition, basis = "bspline", nbasis = 5),
                      data = des, block = ~run, sampling_frame = sframe)

  # Test filtering to basis 2-3
  con <- pair_contrast(~ condition == "Face", ~ condition == "Scene",
                      basis = 2:3, name = "Face_vs_Scene_b23")
  term1 <- terms(espec)[[1]]
  cw <- contrast_weights(con, term1)

  # Should only have basis 2 and 3 conditions
  expect_true(all(grepl("_b0[23]$", cw$condnames)))
  expect_equal(length(cw$condnames), 4)  # Face_b02, Scene_b02, Face_b03, Scene_b03

  # Check non-zero weights
  wvec <- as.vector(cw$weights)
  nonzero_idx <- which(wvec != 0)
  expect_equal(length(nonzero_idx), 4)  # 2 conditions × 2 basis functions
})

test_that("oneway_contrast with basis filtering", {
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    condition = rep(c("A", "B", "C"), 10),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  espec <- event_model(onset ~ hrf(condition, basis = "bspline", nbasis = 5),
                      data = des, block = ~run, sampling_frame = sframe)

  # Test oneway with basis 1 only
  con <- oneway_contrast(~ condition, basis = 1, name = "Main_b1")
  term1 <- terms(espec)[[1]]
  cw <- contrast_weights(con, term1)

  # Should only have basis 1 conditions
  expect_true(all(grepl("_b01$", cw$condnames)))
  expect_equal(length(cw$condnames), 3)  # A_b01, B_b01, C_b01

  # Oneway produces F-contrast, so check matrix dimensions
  expect_equal(nrow(cw$weights), 15)  # Total expanded conditions
  expect_equal(ncol(cw$weights), 2)   # Degrees of freedom for 3-level factor

  # Check that non-zero weights are only in basis 1 rows
  nonzero_rows <- apply(cw$weights, 1, function(x) any(x != 0))
  nonzero_conds <- rownames(cw$weights)[nonzero_rows]
  expect_true(all(grepl("_b01$", nonzero_conds)))
})

test_that("oneway_contrast with basis filtering - multiple basis", {
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    condition = rep(c("A", "B", "C"), 10),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  espec <- event_model(onset ~ hrf(condition, basis = "bspline", nbasis = 5),
                      data = des, block = ~run, sampling_frame = sframe)

  # Test oneway with basis 1-3 (early response)
  con <- oneway_contrast(~ condition, basis = 1:3, name = "Main_early")
  term1 <- terms(espec)[[1]]
  cw <- contrast_weights(con, term1)

  # Should have basis 1, 2, and 3 conditions
  expect_true(all(grepl("_b0[123]$", cw$condnames)))
  expect_equal(length(cw$condnames), 9)  # 3 conditions × 3 basis functions
})

test_that("poly_contrast with basis filtering", {
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    intensity = rep(c("low", "medium", "high"), 10),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  espec <- event_model(onset ~ hrf(intensity, basis = "bspline", nbasis = 5),
                      data = des, block = ~run, sampling_frame = sframe)

  # Test poly with basis 1 only
  con <- poly_contrast(~ intensity, degree = 1, basis = 1,
                      value_map = list(low = 1, medium = 2, high = 3),
                      name = "Linear_b1")
  term1 <- terms(espec)[[1]]
  cw <- contrast_weights(con, term1)

  # Should only have basis 1 conditions
  expect_true(all(grepl("_b01$", cw$condnames)))
  expect_equal(length(cw$condnames), 3)  # low_b01, medium_b01, high_b01

  # Check polynomial weights are applied correctly
  nonzero_rows <- apply(cw$weights, 1, function(x) any(x != 0))
  nonzero_conds <- rownames(cw$weights)[nonzero_rows]
  expect_true(all(grepl("_b01$", nonzero_conds)))
})

test_that("basis filtering with FIR HRF", {
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    condition = rep(c("A", "B"), 15),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  espec <- suppressWarnings(event_model(onset ~ hrf(condition, basis = "fir", nbasis = 10),
                      data = des, block = ~run, sampling_frame = sframe))

  # Test filtering to peak response window (bins 3-5)
  con <- pair_contrast(~ condition == "A", ~ condition == "B",
                      basis = 3:5, name = "A_vs_B_peak")
  term1 <- terms(espec)[[1]]
  cw <- contrast_weights(con, term1)

  # Should only have basis 3, 4, 5 conditions
  expect_true(all(grepl("_b0[345]$", cw$condnames)))
  expect_equal(length(cw$condnames), 6)  # 2 conditions × 3 basis functions
})

test_that("basis filtering handles edge cases", {
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    condition = rep(c("A", "B"), 15),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)

  # Test with basis = NULL (should use all basis functions)
  espec_multi <- event_model(onset ~ hrf(condition, basis = "bspline", nbasis = 5),
                            data = des, block = ~run, sampling_frame = sframe)
  con_null <- pair_contrast(~ condition == "A", ~ condition == "B",
                           basis = NULL, name = "A_vs_B_all")
  term2 <- terms(espec_multi)[[1]]
  cw_null <- contrast_weights(con_null, term2)

  # Should have all 10 conditions (2 conditions × 5 basis)
  expect_equal(length(cw_null$condnames), 10)

  # Test with basis = "all" (should use all basis functions)
  con_all <- pair_contrast(~ condition == "A", ~ condition == "B",
                          basis = "all", name = "A_vs_B_all2")
  cw_all <- contrast_weights(con_all, term2)
  expect_equal(length(cw_all$condnames), 10)
})

test_that("basis filtering validates input", {
  # Test invalid basis index (negative)
  expect_error(
    pair_contrast(~ condition == "A", ~ condition == "B",
                 basis = -1, name = "invalid"),
    "basis must be NULL, 'all', or a positive integer vector"
  )

  # Test invalid basis type (not numeric)
  expect_error(
    pair_contrast(~ condition == "A", ~ condition == "B",
                 basis = "invalid", name = "invalid"),
    "basis must be NULL, 'all', or a positive integer vector"
  )

  # Test that basis indices beyond nbasis are caught at weight computation time
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    condition = rep(c("A", "B"), 15),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  espec <- event_model(onset ~ hrf(condition, basis = "bspline", nbasis = 5),
                      data = des, block = ~run, sampling_frame = sframe)

  # Basis index 10 is beyond nbasis=5, so it should throw an error
  con <- pair_contrast(~ condition == "A", ~ condition == "B",
                      basis = 10, name = "invalid_index")
  term1 <- terms(espec)[[1]]

  # This should generate an error about basis index being beyond nbasis
  expect_error(
    cw <- contrast_weights(con, term1),
    "basis must be NULL, 'all', or integer vector with values in 1:5"
  )
})

test_that("basis filtering preserves sum-to-zero property", {
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    condition = rep(c("A", "B"), 15),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  espec <- event_model(onset ~ hrf(condition, basis = "bspline", nbasis = 5),
                      data = des, block = ~run, sampling_frame = sframe)

  # Test that filtered weights still sum to zero
  con <- pair_contrast(~ condition == "A", ~ condition == "B",
                      basis = 2:4, name = "A_vs_B_mid")
  term1 <- terms(espec)[[1]]
  cw <- contrast_weights(con, term1)

  # For pair_contrast, weights should sum to zero
  expect_equal(sum(cw$weights), 0, tolerance = 1e-8)

  # Check only non-zero weights (filtered basis functions)
  nonzero_weights <- cw$weights[cw$weights != 0]
  expect_equal(sum(nonzero_weights), 0, tolerance = 1e-8)
})

# ====================================================================
# Tests for basis_weights functionality
# ====================================================================

test_that("pair_contrast with basis_weights - basic functionality", {
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    condition = rep(c("A", "B", "C"), 10),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  espec <- event_model(onset ~ hrf(condition, basis = "bspline", nbasis = 5),
                      data = des, block = ~run, sampling_frame = sframe)

  # Test with Gaussian-like weighting emphasizing middle basis
  con <- pair_contrast(~ condition == "A", ~ condition == "B",
                      basis = 1:3,
                      basis_weights = c(0.1, 0.8, 0.1),
                      name = "A_vs_B_weighted")
  term1 <- terms(espec)[[1]]
  cw <- contrast_weights(con, term1)

  # Should have filtered to 3 basis functions for each condition
  expect_true(all(grepl("_b0[123]$", cw$condnames)))
  expect_equal(length(cw$condnames), 9)  # 3 conditions × 3 bases

  # Weights should still sum to zero (contrast property)
  expect_equal(sum(cw$weights), 0, tolerance = 1e-8)

  # Check that basis 2 has higher weight magnitude than basis 1 and 3
  # Extract weights for condition A (note: rownames use "condition.A" with dot)
  A_b01_weight <- abs(cw$weights[rownames(cw$weights) == "condition.A_b01", 1])
  A_b02_weight <- abs(cw$weights[rownames(cw$weights) == "condition.A_b02", 1])
  A_b03_weight <- abs(cw$weights[rownames(cw$weights) == "condition.A_b03", 1])

  # Verify that the weighted basis 2 has the highest magnitude
  expect_true(A_b02_weight > A_b01_weight,
              info = sprintf("b02 (%.4f) should be > b01 (%.4f)", A_b02_weight, A_b01_weight))
  expect_true(A_b02_weight > A_b03_weight,
              info = sprintf("b02 (%.4f) should be > b03 (%.4f)", A_b02_weight, A_b03_weight))

  # The ratio should be approximately 0.1 : 0.8 : 0.1 = 1 : 8 : 1
  expect_equal(A_b02_weight / A_b01_weight, 8, tolerance = 0.1)
  expect_equal(A_b02_weight / A_b03_weight, 8, tolerance = 0.1)
})

test_that("basis_weights auto-normalization with warning", {
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    condition = rep(c("A", "B"), 15),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  espec <- event_model(onset ~ hrf(condition, basis = "bspline", nbasis = 5),
                      data = des, block = ~run, sampling_frame = sframe)

  # Weights that sum to 2 should trigger normalization warning
  con <- pair_contrast(~ condition == "A", ~ condition == "B",
                      basis = 1:2,
                      basis_weights = c(1, 1),
                      name = "A_vs_B_norm")
  term1 <- terms(espec)[[1]]

  expect_warning(
    cw <- contrast_weights(con, term1),
    "basis_weights sum to 2.000000, normalizing to sum to 1.0"
  )

  # After normalization, should work correctly
  expect_equal(sum(cw$weights), 0, tolerance = 1e-8)
})

test_that("basis_weights validation - length mismatch", {
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    condition = rep(c("A", "B"), 15),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  espec <- event_model(onset ~ hrf(condition, basis = "bspline", nbasis = 5),
                      data = des, block = ~run, sampling_frame = sframe)

  # 3 bases selected but only 2 weights provided
  con <- pair_contrast(~ condition == "A", ~ condition == "B",
                      basis = 1:3,
                      basis_weights = c(0.5, 0.5),
                      name = "A_vs_B_mismatch")
  term1 <- terms(espec)[[1]]

  expect_error(
    cw <- contrast_weights(con, term1),
    "basis_weights length \\(2\\) must match number of selected basis functions \\(3\\)"
  )
})

test_that("basis_weights validation - negative weights", {
  # Should fail at construction time
  expect_error(
    pair_contrast(~ condition == "A", ~ condition == "B",
                 basis = 1:2,
                 basis_weights = c(0.5, -0.5),
                 name = "A_vs_B_neg"),
    "basis_weights must be non-negative"
  )
})

test_that("basis_weights validation - NA values", {
  # Should fail at construction time
  expect_error(
    pair_contrast(~ condition == "A", ~ condition == "B",
                 basis = 1:2,
                 basis_weights = c(0.5, NA),
                 name = "A_vs_B_na"),
    "basis_weights must be a numeric vector without NAs"
  )
})

test_that("basis_weights validation - non-numeric", {
  # Should fail at construction time
  expect_error(
    pair_contrast(~ condition == "A", ~ condition == "B",
                 basis = 1:2,
                 basis_weights = c("0.5", "0.5"),
                 name = "A_vs_B_char"),
    "basis_weights must be a numeric vector without NAs"
  )
})

test_that("oneway_contrast with basis_weights", {
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    condition = rep(c("A", "B", "C"), 10),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  espec <- event_model(onset ~ hrf(condition, basis = "bspline", nbasis = 5),
                      data = des, block = ~run, sampling_frame = sframe)

  # Test with weighted early response
  con <- oneway_contrast(~ condition,
                        basis = 1:3,
                        basis_weights = c(0.5, 0.3, 0.2),
                        name = "Main_weighted")
  term1 <- terms(espec)[[1]]
  cw <- contrast_weights(con, term1)

  # Should filter to 3 bases per condition
  expect_true(all(grepl("_b0[123]$", cw$condnames)))
  expect_equal(length(cw$condnames), 9)  # 3 conditions × 3 bases

  # This is an F-contrast, so multiple columns
  expect_equal(ncol(cw$weights), 2)  # 3 conditions - 1 = 2 df
})

test_that("poly_contrast with basis_weights", {
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    intensity = rep(c("low", "med", "high"), 10),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  espec <- event_model(onset ~ hrf(intensity, basis = "bspline", nbasis = 5),
                      data = des, block = ~run, sampling_frame = sframe)

  # Test linear trend with weighted bases
  con <- poly_contrast(~ intensity,
                      degree = 1,
                      value_map = list(low = 1, med = 2, high = 3),
                      basis = 1:2,
                      basis_weights = c(0.7, 0.3),
                      name = "Linear_weighted")
  term1 <- terms(espec)[[1]]
  cw <- contrast_weights(con, term1)

  # Should filter to 2 bases per level
  expect_true(all(grepl("_b0[12]$", cw$condnames)))
  expect_equal(length(cw$condnames), 6)  # 3 levels × 2 bases

  # Polynomial contrast returns single column
  expect_equal(ncol(cw$weights), 1)
})

test_that("basis_weights with basis = NULL (all bases)", {
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    condition = rep(c("A", "B"), 15),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  espec <- event_model(onset ~ hrf(condition, basis = "bspline", nbasis = 5),
                      data = des, block = ~run, sampling_frame = sframe)

  # When basis = NULL, basis_weights should apply to all 5 bases
  con <- pair_contrast(~ condition == "A", ~ condition == "B",
                      basis = NULL,
                      basis_weights = c(0.1, 0.2, 0.3, 0.25, 0.15),
                      name = "A_vs_B_all_weighted")
  term1 <- terms(espec)[[1]]
  cw <- contrast_weights(con, term1)

  # Should have all 5 bases (2 conditions × 5 bases = 10)
  expect_equal(length(cw$condnames), 10)  # 2 conditions × 5 bases
  expect_true(any(grepl("_b05$", cw$condnames)))
})

test_that("basis_weights preserves sum-to-zero property", {
  des <- data.frame(
    onset = seq(2, 120, by = 4),
    condition = rep(c("A", "B", "C", "D"), length.out = 30),
    run = rep(1:2, each = 15)
  )
  sframe <- fmrihrf::sampling_frame(blocklens = c(50, 50), TR = 2)
  espec <- event_model(onset ~ hrf(condition, basis = "bspline", nbasis = 5),
                      data = des, block = ~run, sampling_frame = sframe)

  # Test with various weight patterns
  weight_patterns <- list(
    c(0.2, 0.8),           # Emphasize late
    c(0.9, 0.1),           # Emphasize early
    c(0.33, 0.34, 0.33),   # Uniform-ish
    c(0.1, 0.6, 0.3)       # Middle emphasis
  )

  for (i in seq_along(weight_patterns)) {
    weights <- weight_patterns[[i]]
    basis_sel <- seq_along(weights)

    con <- pair_contrast(~ condition == "A", ~ condition == "B",
                        basis = basis_sel,
                        basis_weights = weights,
                        name = paste0("test_", i))
    term1 <- terms(espec)[[1]]
    cw <- contrast_weights(con, term1)

    # Sum-to-zero property must hold
    expect_equal(sum(cw$weights), 0, tolerance = 1e-8,
                info = paste("Pattern", i, "failed sum-to-zero"))
  }
})
