#' Initialize portfolio
#'
#' Initialize portfolio with given currency and a cash amount.
#'
#' @param currency three-letter currency (string).
#' @param init_cash initial cash (number).
#'
#' @return A portfolio.
#'
#' @examples
#' pfo_init('USD', 100000)
#'
#' @export
pfo_init <- function(currency, init_cash) {
  assertthat::assert_that(assertthat::is.string(currency))
  assertthat::assert_that(assertthat::is.number(init_cash))
  lst <- list()
  lst[[currency]] <- init_cash
  pfo_from_list(currency, lst)
}

#' Initialize portfolio from list
#'
#' Initialize portfolio with given currency from a list.
#'
#' @param currency three-letter currency (string).
#' @param lst list of instrument quantities indexed by instrument tickers.
#'
#' @return A portfolio.
#'
#' @examples
#' pfo_from_list('USD', list('USD' = 13937.10, 'MSCI' = 12.08, 'VIX' = 20))
#'
#' @export
pfo_from_list <- function(currency, lst) {
  assertthat::assert_that(assertthat::is.string(currency))
  assertthat::assert_that(is.list(lst))
  ret <- lst
  attr(ret, 'currency') <- currency
  return(ret)
}

#' Portfolio currency
#'
#' Portfolio currency.
#'
#' @param pfo portfoio.
#'
#' @return A string.
#'
#' @examples
#' p1 <- pfo_init('USD', 100000)
#' pfo_currency(p1)
#'
#' @export
pfo_currency <- function(pfo) {
  currency <- attr(pfo, 'currency', exact = TRUE)
  assertthat::assert_that(!is.null(currency))
  assertthat::assert_that(assertthat::is.string(currency))
  return(currency)
}


#' Make a trade in portfolio
#'
#' Make a trade in portfolio, given instrument, traded quantity
#' and price in portfolio currency.
#'
#' @param pfo input portfolio.
#' @param instrument financial instrument (string).
#' @param trade_qty trade quantity (number).
#' @param price price in portfolio currency (number).
#'
#' @return A portfolio.
#'
#' @examples
#' p1 <- pfo_init('USD', 100000)
#' pfo_trade(p1, 'MSCI', 10.5, 1756)
#'
#' @export
pfo_trade <- function(pfo, instrument, trade_qty, price) {
  assertthat::assert_that(assertthat::is.string(instrument))
  assertthat::assert_that(assertthat::is.number(trade_qty))
  assertthat::assert_that(assertthat::is.number(price))

  currency <- attr(pfo, 'currency', exact = TRUE)
  assertthat::assert_that(!is.null(currency))

  cash_change <- -trade_qty*price
  ret <- pfo
  ret <- pfo_add(ret, currency, cash_change)
  ret <- pfo_add(ret, instrument, trade_qty)

  return(ret)
}

#' Make a trade in portfolio to a target qty
#'
#' Make a trade in portfolio, given an instrument,
#' its target final quantity and the price of the instrument
#' in portfolio currency.
#'
#' @param pfo input portfolio.
#' @param instrument financial instrument (string).
#' @param target_qty target quantity (number).
#' @param price price in portfolio currency (number).
#'
#' @return A portfolio.
#'
#' @examples
#' p1 <- pfo_init('USD', 100000)
#' p2 <- pfo_trade(p1, 'MSCI', 10.5, 1756)
#' p3 <- pfo_trade(p2, 'MSCI', 12.5, 1789)
#' p4 <- pfo_trade(p3, 'MSCI', 0, 1793)
#'
#' @export
pfo_target_trade <- function(pfo, instrument, target_qty, price) {
  assertthat::assert_that(assertthat::is.number(target_qty))
  existing_qty <- pfo_instr_qty(pfo, instrument)
  trade_qty <- target_qty - existing_qty
  pfo_trade(pfo, instrument, trade_qty, price)
}


pfo_add <- function(pfo, instrument, trade_qty) {
  already_in <- exists(instrument, pfo)
  ret <- pfo
  final_qty <- trade_qty + (if (already_in) ret[[instrument]] else 0)
  is_zero <- dplyr::near(final_qty, 0)
  ret[[instrument]] <- if(is_zero) NULL else final_qty
  return(ret)
}

#' Calculate nav of pfo given price list/env
#'
#' Calculate nav of portfolio given price list or environment.
#'
#' @param pfo input portfolio.
#' @param pl price list or environment.
#'
#' @return A number.
#'
#' @examples
#' p1 <- pfo_init('USD', 100000)
#' p2 <- pfo_trade(p1, 'MSCI', 10.5, 1756)
#' pl <- list('MSCI' = 1798)
#' pfo_nav(p2, pl)
#'
#' @export
pfo_nav <- function(pfo, pl) {
  currency <- pfo_currency(pfo)
  pl[[currency]] <- 1
  instrs <- names(pfo)
  assertthat::assert_that(all(instrs %in% names(pl)))
  sum(purrr::map_dbl(.x = instrs, .f = function(instr) pfo[[instr]]*pl[[instr]]))
}

#' Quantity of an instrument in a portfolio
#'
#' Quantity of an instrument in a portfolio.
#'
#' @param pfo input portfolio.
#' @param instr instrument identifier (string).
#'
#' @return A number.
#'
#' @examples
#' p1 <- pfo_init('USD', 100000)
#' p2 <- pfo_trade(p1, 'MSCI', 10.5, 1756)
#' pfo_instr_qty(p2, 'USD')
#' pfo_instr_qty(p2, 'MSCI')
#' pfo_instr_qty(p2, 'EUR')
#'
#' @export
pfo_instr_qty <- function(pfo, instr) {
  assertthat::assert_that(assertthat::is.string(instr))
  val <- pfo[[instr]]
  if (is.null(val)) 0 else val
}

#' Instruments in a portfolio
#'
#' Instruments in a portfolio.
#'
#' @param pfo input portfolio.
#'
#' @return A character vector.
#'
#' @examples
#' p1 <- pfo_init('USD', 100000)
#' p2 <- pfo_trade(p1, 'MSCI', 10.5, 1756)
#' pfo_instruments(p1)
#' pfo_instruments(p2)
#'
#' @export
pfo_instruments <- function(pfo) {
  names(pfo)
}

#' Instruments in a portfolio except pfo ccy
#'
#' Instruments in a portfolio except portfolio currency.
#'
#' @param pfo input portfolio.
#'
#' @return A character vector.
#'
#' @examples
#' p1 <- pfo_init('USD', 100000)
#' p2 <- pfo_trade(p1, 'MSCI', 10.5, 1756)
#' pfo_instruments_except_pfo_ccy(p1)
#' pfo_instruments_except_pfo_ccy(p2)
#'
#' @export
pfo_instruments_except_pfo_ccy <- function(pfo) {
  setdiff(pfo_instruments(pfo), pfo_currency(pfo))
}

#' Get prices of instruments in pfo
#'
#' Get prices of instruments in pfo
#'
#' @param pfo input portfolio.
#' @param dt date.
#' @param xts_val_pri.e environment of xts timeseries
#'   indexed by ticker.
#'
#' @return A named numeric vector.
#'
#' @examples
#' p1 <- pfo_init('USD', 100000)
#' p2 <- pfo_trade(p1, 'MSCI', 10.5, 1756)
#' dt <- as.Date('2019-10-02')
#' tsmsci <- xts::xts(x = c(2121.47), dt)
#' xtslst <- list('MSCI' = tsmsci)
#' xtsenv <- list2env(xtslst)
#' pfo_instr_prices_on(p1, dt, xtsenv)
#' pfo_instr_prices_on(p2, dt, xtsenv)
#'
#' @export
pfo_instr_prices_on <- function(pfo, dt, xts_val_pri.e) {
  instr.v <- pfo_instruments_except_pfo_ccy(pfo)
  retl <- lapply(instr.v, function(tic) instr_price_on(xts_val_pri.e, tic, dt))
  names(retl) <- instr.v
  return(retl)
}

#' Get price of instrument on specific date
#'
#' Get price of instrument on specific date. Extract price from
#' environment of timeseries.
#'
#' @param xts_pri_env environment of xts timeseries
#'   indexed by ticker.
#' @param tic instrument ticker.
#' @param dt date.
#'
#' @return A number.
#'
#' @examples
#' dt <- as.Date('2019-10-02')
#' tsmsci <- xts::xts(x = c(2121.47), dt)
#' xtslst <- list('MSCI' = tsmsci)
#' xtsenv <- list2env(xtslst)
#' instr_price_on(xtsenv, 'MSCI', dt)
#'
#' @export
instr_price_on <- function(xts_pri_env, tic, dt) {
  price <- as.double(xts_pri_env[[tic]][dt])
  is_number_price <- assertthat::is.number(price)
  if (!is_number_price) {
    stop('Bad ', tic, ' price on ', dt)
  }
  return(price)
}


#' Compute nav of pfo on specific date
#'
#' Compute nav of portfolio on specific date.
#'
#' @param pfo input portfolio.
#' @param dt date.
#' @param xts_val_pri.e environment of xts timeseries
#'   indexed by ticker.
#'
#' @return A number.
#'
#' @examples
#' p1 <- pfo_init('USD', 100000)
#' p2 <- pfo_trade(p1, 'MSCI', 10.5, 1756)
#' dt <- as.Date('2019-10-02')
#' tsmsci <- xts::xts(x = c(2121.47), dt)
#' xtslst <- list('MSCI' = tsmsci)
#' xtsenv <- list2env(xtslst)
#' pfo_nav_on(p1, dt, xtsenv)
#' pfo_nav_on(p2, dt, xtsenv)
#'
#' @export
pfo_nav_on <- function(pfo, dt, xts_val_pri.e) {
  pl <- pfo_instr_prices_on(pfo, dt, xts_val_pri.e)
  pfo_nav(pfo, pl)
}

#' Description of pfo in string
#'
#' Description of portfolio in string.
#'
#' @param pfo input portfolio.
#'
#' @return A string.
#'
#' @examples
#' p1 <- pfo_init('USD', 100000)
#' p2 <- pfo_trade(p1, 'MSCI', 10.5, 1756)
#' pfo_to_str(p1)
#' pfo_to_str(p2)
#'
#' @export
pfo_to_str <- function(pfo) {
  utils::capture.output(dput(pfo))
}

#' Description of pfo instruments in string
#'
#' Description of portfolio instruments in string.
#'
#' @param pfo input portfolio.
#'
#' @return A string.
#'
#' @examples
#' p1 <- pfo_init('USD', 100000)
#' p2 <- pfo_trade(p1, 'MSCI', 10.5, 1756)
#' pfo_instr_str(p1)
#' pfo_instr_str(p2)
#'
#' @export
pfo_instr_str <- function(pfo) {
  attr(pfo, 'currency') <- NULL
  utils::capture.output(dput(pfo))
}

#' Get subset portfolio with instrument grepl
#'
#' Get subset portfolio with instrument grepl.
#'
#' @param pfo input portfolio.
#' @param pattern regular expression string.
#'
#' @return A portfolio.
#'
#' @examples
#' p1 <- pfo_init('USD', 100000)
#' p2 <- pfo_trade(p1, 'MSCI', 10.5, 1756)
#' pfo_grepl(p1, '^MSCI$')
#' pfo_grepl(p2, '^MSCI$')
#' pfo_grepl(p2, '.')
#'
#' @export
pfo_grepl <- function(pfo, pattern) {
  retpfo <- pfo[grepl(pattern, pfo_instruments(pfo))]
  attr(retpfo, 'currency') <- pfo_currency(pfo)
  return(retpfo)
}


#' Make trades in a portfolio
#'
#' Make trades in portfolio from a list of trades.
#'
#' @param pfo input portfolio.
#' @param trade_list trade list.
#'
#' @return A portfolio.
#'
#' @examples
#' p1 <- pfo_init('USD', 100000)
#' trade_list <- list(
#'   list(instrument = 'MSCI', trade_qty = 10, price = 1756),
#'   list(instrument = 'MSCI', trade_qty = 15, price = 1700)
#' )
#' pfo_make_trades(p1, trade_list)
#'
#' @export
pfo_make_trades <- function(pfo, trade_list) {
  assertthat::assert_that(is.list(trade_list))
  for (trd in trade_list) {
    args <- c(list('pfo' = pfo), trd)
    pfo <- do.call(what = pfo_trade, args = args)
  }
  return(pfo)
}


#' Compute trades between a start and a target pfo
#'
#' Compute trades between a start and a target pfo.
#'
#' @param from initial portfolio.
#' @param to target portfolio.
#' @param dt date.
#' @param val_pri.e price values environment.
#'
#' @return A list of trades.
#'
#' @examples
#' p1 <- pfo_init('USD', 100000)
#' p2 <- pfo_trade(p1, 'MSCI', 10.5, 1756)
#' p3 <- pfo_trade(p2, 'VIX', 20, 14)
#' from <- pfo_grepl(p1, 'MSCI|VIX')
#' to <- pfo_grepl(p3, 'MSCI|VIX')
#' dt <- as.Date('2019-05-22')
#' xts_msci <- xts::xts(1756, order.by = dt)
#' xts_vix <- xts::xts(14, order.by = dt)
#' val_pri.l <- list('MSCI' = xts_msci, 'VIX' = xts_vix)
#' val_pri.e <- list2env(val_pri.l)
#' diff_trades(from, to, dt, val_pri.e)
#'
#' @export
diff_trades <- function(from, to, dt, val_pri.e) {
  dp <- diff_pfo(from, to)
  purrr::map2(.x = names(dp), .y = dp, .f = function(tic, qty) {
    list('instrument' = tic,
         'trade_qty' = qty,
         'price' = instr_price_on(val_pri.e, tic, dt))
  })
}


#' Difference between two portfolios
#'
#' Difference between two portfolios: difference from `from` to `to`.
#'
#' @param from initial portfolio.
#' @param to target portfolio.
#'
#' @return A named list of differences indexed by ticker.
#'
#' @examples
#' p1 <- pfo_init('USD', 100000)
#' p2 <- pfo_trade(p1, 'MSCI', 10.5, 1756)
#' p3 <- pfo_trade(p2, 'VIX', 20, 14)
#' from <- pfo_grepl(p1, 'MSCI|VIX')
#' to <- pfo_grepl(p3, 'MSCI|VIX')
#' diff_pfo(from, to)
#'
#' @export
diff_pfo <- function(from, to) {
  all_instr <- union(pfo_instruments(from), pfo_instruments(to))
  retlst <- list()
  for (instr in all_instr) {
    to_qty <- if(is.null(to[[instr]])) 0.0 else to[[instr]]
    from_qty <- if(is.null(from[[instr]])) 0.0 else from[[instr]]
    di <- to_qty - from_qty
    if (!dplyr::near(di, 0.0)) { retlst[[instr]] <- di }
  }
  return(retlst)
}
