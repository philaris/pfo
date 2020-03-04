testthat::context('portfolio')

p1 <- pfo_init('USD', 100000)

testthat::test_that("portfolio initialize", {
  testthat::expect_equal(p1[['USD']], 100000)
  testthat::expect_equal(attr(p1, 'currency', exact = TRUE), 'USD')
})

p2 <- pfo_trade(p1, 'MSCI', 20, 2109.34)
p3 <- pfo_trade(p2, 'MSCI', 10, 2130)


testthat::test_that("pfo trades from list", {
  tl <- list(
    list('instrument' = 'MSCI', 'trade_qty' = 20, 'price' = 2109.34),
    list('instrument' = 'MSCI', 'trade_qty' = 10, 'price' = 2130)
  )
  ptl <- pfo_make_trades(p1, tl)
  testthat::expect_equal(p3, ptl)
})

testthat::test_that("portfolio currency", {
  testthat::expect_equal(pfo_currency(p1), 'USD')
  testthat::expect_equal(pfo_currency(p2), 'USD')
  testthat::expect_equal(pfo_currency(p3), 'USD')
})

testthat::test_that("portfolio instruments", {
  testthat::expect_equal(pfo_instruments(p1), c('USD'))
  testthat::expect_equal(pfo_instruments_except_pfo_ccy(p1), character(0))
  testthat::expect_equal(pfo_instruments_except_pfo_ccy(p2), c('MSCI'))
  testthat::expect_setequal(pfo_instruments(p2), c('USD', 'MSCI'))
})

testthat::test_that("portfolio trade", {
  testthat::expect_equal(p2[['MSCI']], 20)
  testthat::expect_equal(p2[['USD']], 100000-20*2109.34)
  testthat::expect_equal(p3[['MSCI']], 30)
  testthat::expect_equal(p3[['USD']], 100000-20*2109.34-10*2130)
})

testthat::test_that("portfolio nav simple", {
  testthat::expect_equal(pfo_nav(p1, list()), 100000)
})

testthat::test_that("portfolio nav with MSCI", {
  testthat::expect_equal(pfo_nav(p2, list('MSCI' = 2109.34)), 100000)
  testthat::expect_gt(pfo_nav(p2, list('MSCI' = 2130.00)), 100000)
  testthat::expect_lt(pfo_nav(p2, list('MSCI' = 2098.00)), 100000)
})

testthat::test_that("portfolio quantity", {
  testthat::expect_equal(pfo_instr_qty(p1, 'EUR'), 0)
  testthat::expect_equal(pfo_instr_qty(p1, 'MSCI'), 0)
  testthat::expect_equal(pfo_instr_qty(p2, 'MSCI'), 20)
  pem <- pfo_trade(p2, 'MSCI', -20, 2101.71)
  testthat::expect_equal(pfo_instr_qty(pem, 'MSCI'), 0)
  testthat::expect_equal(pfo_instr_qty(pem, 'EUR'), 0)
  testthat::expect_gt(pfo_instr_qty(pem, 'USD'), 0)
})


testthat::test_that("portfolio trade to target", {
  testthat::expect_equal(pfo_instr_qty(p3, 'MSCI'), 30)
  p4 <- pfo_target_trade(p3, 'MSCI', 50, 2100)
  testthat::expect_equal(pfo_instr_qty(p4, 'MSCI'), 50)
  p5 <- pfo_target_trade(p4, 'MSCI', 0, 2105)
  testthat::expect_equal(pfo_instr_qty(p5, 'MSCI'), 0)
})

testthat::test_that("portfolio trade to target same price", {
  pa <- pfo_init('USD', 100000)
  pb <- pfo_target_trade(pa, 'MSCI', 30, 2100)
  pc <- pfo_target_trade(pb, 'MSCI', 0, 2100)
  testthat::expect_equal(pfo_instr_qty(pc, 'MSCI'), 0)
  testthat::expect_equal(pfo_instr_qty(pc, 'USD'), 100000)
})


dt <- as.Date('2019-10-02')
tsmsci <- xts::xts(x = c(2121.47), dt)
xtslempty <- list()
envempty <- list2env(xtslempty)
xtsl1 <- list('MSCI' = tsmsci)
env1 <- list2env(xtsl1)

testthat::test_that("instrument price", {
  testthat::expect_equal(instr_price_on(env1, 'MSCI', dt), 2121.47)
  testthat::expect_error(instr_price_on(env1, 'MSCI', dt + 1L))
  testthat::expect_error(instr_price_on(env1, 'BAD_TIC', dt))
})

testthat::test_that("portfolio extract prices on given date", {
  eli <- list() %>% magrittr::set_names(character(0L))
  testthat::expect_equal(pfo_instr_prices_on(p1, dt, envempty), eli)
  testthat::expect_equal(pfo_instr_prices_on(p1, dt, env1), eli)
  testthat::expect_error(pfo_instr_prices_on(p2, dt, envempty))
  testthat::expect_equal(pfo_instr_prices_on(p2, dt, env1), list('MSCI' = 2121.47))
})

testthat::test_that("portfolio nav on given date", {
  testthat::expect_equal(pfo_nav_on(p1, dt, envempty), 100000)
  testthat::expect_equal(pfo_nav_on(p1, dt, env1), 100000)
  testthat::expect_error(pfo_nav_on(p2, dt, envempty))
  testthat::expect_gt(pfo_nav_on(p2, dt, env1), 100000)
})

testthat::test_that("portfolio descriptions in string", {
  testthat::expect_true(is.character(pfo_to_str(p1)))
  testthat::expect_true(is.character(pfo_to_str(p2)))
  testthat::expect_true(is.character(pfo_instr_str(p1)))
  testthat::expect_true(is.character(pfo_instr_str(p2)))
})

testthat::test_that("pfo grepl", {
  testthat::expect_equal(p1, pfo_grepl(p1, '.'))
  testthat::expect_equal(p2, pfo_grepl(p2, '.'))
  testthat::expect_length(pfo_grepl(p1, '^MSCI'), 0L)
  testthat::expect_length(pfo_grepl(p2, '^MSCI'), 1L)
})

testthat::test_that("pfo diff and trades", {
  p1 <- pfo_init('USD', 100000)
  p2 <- pfo_trade(p1, 'MSCI', 10.5, 1756)
  p3 <- pfo_trade(p2, 'VIX', 20, 14)
  from <- pfo_grepl(p1, 'MSCI|VIX')
  to <- pfo_grepl(p3, 'MSCI|VIX')
  ds <- diff_pfo(from, to)
  testthat::expect_equal(ds, list('MSCI' = 10.5, 'VIX' = 20))
  dsinv <- diff_pfo(to, from)
  testthat::expect_equal(dsinv, list('MSCI' = -10.5, 'VIX' = -20))

  dt <- as.Date('2019-05-22')
  xts_msci <- xts::xts(1756, order.by = dt)
  xts_vix <- xts::xts(14, order.by = dt)
  val_pri.l <- list('MSCI' = xts_msci, 'VIX' = xts_vix)
  val_pri.e <- list2env(val_pri.l)
  trds <- diff_trades(from, to, dt, val_pri.e)
  testthat::expect_equal(trds,
                         list(list(instrument = "MSCI", trade_qty = 10.5, price = 1756),
                              list(instrument = "VIX", trade_qty = 20, price = 14)))
  trdsinv <- diff_trades(to, from, dt, val_pri.e)
  testthat::expect_equal(trdsinv,
                         list(list(instrument = "MSCI", trade_qty = -10.5, price = 1756),
                              list(instrument = "VIX", trade_qty = -20, price = 14)))
})
