test_that("confmatr_by_threshold calculates all metrics correctly", {
  test_vec <- c(0.8, 0.7, 0.6, 0.3, 0.4, 0.2)
  true_vec <- c(1L, 1L, 0L, 0L, 1L, 0L)
  res <- confmatr_by_threshold(test_vec, true_vec, threshold = 0.5, pos_val = 1)

  expect_equal(res$threshold, 0.5)                     # evaluated at cutoff 0.5
  expect_equal(res$P, 3)                                # three positives in truth vector
  expect_equal(res$N, 3)                                # three negatives in truth vector
  expect_equal(res$TP, 2)                               # two positives above threshold
  expect_equal(res$FN, 1)                               # one positive below threshold
  expect_equal(res$FP, 1)                               # one negative above threshold
  expect_equal(res$TN, 2)                               # two negatives below threshold
  expect_equal(res$TPR, 2/3)                            # TP / P
  expect_equal(res$SEN, 2/3)                            # alias for TPR
  expect_equal(res$FNR, 1/3)                            # 1 - TPR
  expect_equal(res$FPR, 1/3)                            # FP / N
  expect_equal(res$TNR, 2/3)                            # 1 - FPR
  expect_equal(res$SPC, 2/3)                            # alias for TNR
  expect_equal(res$BM, 1/3)                             # TPR + TNR - 1
  expect_equal(res$PT, sqrt(2) - 1)                     # probability threshold formula
  expect_equal(res$Prevalence, 0.5)                     # P / (P + N)
  expect_equal(res$PPV, 2/3)                            # TP / (TP + FP)
  expect_equal(res$NPV, 2/3)                            # TN / (TN + FN)
  expect_equal(res$LRp, 2)                              # TPR / FPR
  expect_equal(res$LRn, 1/2)                            # FNR / TNR
  expect_equal(res$ACC, 4/6)                            # (TP + TN) / (P + N)
  expect_equal(res$FDR, 1/3)                            # 1 - PPV
  expect_equal(res$FOR, 1/3)                            # 1 - NPV
  expect_equal(res$MK, 1/3)                             # PPV + NPV - 1
  expect_equal(res$deltaP, 1/3)                         # deltaP equals markedness
  expect_equal(res$DOR, 4)                              # LRp / LRn
  expect_equal(res$BA, 2/3)                             # (TPR + TNR) / 2
  expect_equal(res$F_1, 2/3)                            # 2 * PPV * TPR / (PPV + TPR)
  expect_equal(res$FM, 2/3)                             # sqrt(PPV * TPR)
  expect_equal(res$MCC, 1/3)                            # Matthews correlation coefficient
  expect_equal(res$TS, 1/2)                             # TP / (TP + FN + FP)
  expect_equal(res$CSI, 1/2)                            # critical success index
})

test_that("confmatr_by_threshold handles multiple thresholds and options", {
  test_vec <- c(0.8, 0.7, 0.6, 0.3, 0.4, 0.2)
  true_vec <- c(1L, 1L, 0L, 0L, 1L, 0L)

  res <- confmatr_by_threshold(test_vec, true_vec, threshold = c(0.5, 0.7), pos_val = 1)
  expect_equal(nrow(res), 2)                             # vector thresholds return one row per cutoff

  res_high <- res[res$threshold == 0.7, ]
  expect_equal(res_high$TP, 2)                           # two positives exceed 0.7
  expect_equal(res_high$FP, 0)                           # no negatives exceed 0.7
  expect_equal(res_high$FPR, 0)                          # FP / N = 0
  expect_equal(res_high$TNR, 1)                          # all negatives correctly classified
  expect_equal(res_high$PPV, 1)                          # TP / (TP + FP) with FP=0
  expect_equal(res_high$NPV, 3/4)                        # TN / (TN + FN)
  expect_equal(res_high$LRp, Inf)                        # division by zero when FPR = 0
  expect_equal(res_high$LRn, 1/3)                        # FNR / TNR
  expect_equal(res_high$BA, 5/6)                         # (TPR + TNR) / 2
  expect_equal(res_high$F_1, 4/5)                        # 2 * PPV * TPR / (PPV + TPR)
  expect_equal(res_high$FM, sqrt(2/3))                   # sqrt(PPV * TPR)
  expect_equal(res_high$MCC, sqrt(1/2))                  # simplified with zero FPR
  expect_equal(res_high$TS, 2/3)                         # TP / (TP + FN + FP)
  expect_equal(res_high$CSI, 2/3)                        # critical success index

  res_pref <- confmatr_by_threshold(test_vec, true_vec, threshold = c(0.5, 0.7), pos_val = 1, prepend = "x_")
  expect_true(all(startsWith(names(res_pref), "x_")))    # prefix applied to all columns

  res_cols <- confmatr_by_threshold(test_vec, true_vec, threshold = 0.5, pos_val = 1, cols = c(threshold, TPR, FPR))
  expect_equal(names(res_cols), c("threshold", "TPR", "FPR")) # only selected columns returned
})
