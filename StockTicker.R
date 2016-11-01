DOW.URL <- "http://www.google.com/finance?cid=983582"
DOW.raw <- readLines(url(description = DOW.URL, open = "r"))

NASDAQ.URL <- "http://www.google.com/finance?cid=13756934"
NASDAQ.raw <- readLines(url(description = NASDAQ.URL, open = "r"))

SP.URL <- "http://www.google.com/finance?cid=626307"
SP.raw <- readLines(url(description = SP.URL, open = "r"))

VG.URL <- "http://www.google.com/finance?cid=700341"
VG.raw <- readLines(url(description = VG.URL, open = "r"))

#############################################################################################################

DOW.open.value <- DOW.raw[grep(pattern = '          data-snapfield=\"open\">Open', x = DOW.raw) + 2]
DOW.open.value <- sub(pattern = '<.*.>', replacement = '', x = DOW.open.value)
DOW.open.value <- sub(pattern = ',', replacement = '', x = DOW.open.value)
DOW.open.value <- as.numeric(DOW.open.value)

NASDAQ.open.value <- NASDAQ.raw[grep(pattern = '          data-snapfield=\"open\">Open', x = NASDAQ.raw) + 2]
NASDAQ.open.value <- sub(pattern = '<.*.>', replacement = '', x = NASDAQ.open.value)
NASDAQ.open.value <- sub(pattern = ',', replacement = '', x = NASDAQ.open.value)
NASDAQ.open.value <- as.numeric(NASDAQ.open.value)

SP.open.value <- SP.raw[grep(pattern = '          data-snapfield=\"open\">Open', x = SP.raw) + 2]
SP.open.value <- sub(pattern = '<.*.>', replacement = '', x = SP.open.value)
SP.open.value <- sub(pattern = ',', replacement = '', x = SP.open.value)
SP.open.value <- as.numeric(SP.open.value)

VG.open.value <- VG.raw[grep(pattern = '          data-snapfield=\"open\">Open', x = VG.raw) + 2]
VG.open.value <- sub(pattern = '<.*.>', replacement = '', x = VG.open.value)
VG.open.value <- sub(pattern = ',', replacement = '', x = VG.open.value)
VG.open.value <- as.numeric(VG.open.value)

#############################################################################################################

DOW.chart <- list()
DOW.times <- list()

NASDAQ.chart <- list()
NASDAQ.times <- list()

SP.chart <- list()
SP.times <- list()

VG.chart <- list()
VG.times <- list()

#############################################################################################################

while(TRUE) {

    IO <- ifelse(as.numeric(format(Sys.time(), "%S")) < 10, 1, 0)

    if(IO == 1) {

        #####################################################################################################

        DOW.raw <- readLines(url(description = DOW.URL, open = "r"))
        NASDAQ.raw <- readLines(url(description = NASDAQ.URL, open = "r"))
        SP.raw <- readLines(url(description = SP.URL, open = "r"))
        VG.raw <- readLines(url(description = VG.URL, open = "r"))

        #####################################################################################################

        TIME <- as.POSIXct(Sys.time())

        DOW.times <- unlist(append(DOW.times, TIME))
        NASDAQ.times <- unlist(append(NASDAQ.times, TIME))
        SP.times <- unlist(append(SP.times, TIME))
        VG.times <- unlist(append(VG.times, TIME))

        #####################################################################################################

        DOW.new.value <- DOW.raw[grep('id-price-panel', DOW.raw) + 3]
        DOW.new.value <- sub(pattern = '</span>', replacement = '', x = DOW.new.value)
        DOW.new.value <- sub(pattern = '<.*>', replacement = '', x = DOW.new.value)
        DOW.new.value <- sub(pattern = ',', replacement = '', x = DOW.new.value)
        DOW.new.value <- as.numeric(DOW.new.value)

        NASDAQ.new.value <- NASDAQ.raw[grep('id-price-panel', NASDAQ.raw) + 3]
        NASDAQ.new.value <- sub(pattern = '</span>', replacement = '', x = NASDAQ.new.value)
        NASDAQ.new.value <- sub(pattern = '<.*>', replacement = '', x = NASDAQ.new.value)
        NASDAQ.new.value <- sub(pattern = ',', replacement = '', x = NASDAQ.new.value)
        NASDAQ.new.value <- as.numeric(NASDAQ.new.value)

        SP.new.value <- SP.raw[grep('id-price-panel', SP.raw) + 3]
        SP.new.value <- sub(pattern = '</span>', replacement = '', x = SP.new.value)
        SP.new.value <- sub(pattern = '<.*>', replacement = '', x = SP.new.value)
        SP.new.value <- sub(pattern = ',', replacement = '', x = SP.new.value)
        SP.new.value <- as.numeric(SP.new.value)

        VG.new.value <- VG.raw[grep('id-price-panel', VG.raw) + 3]
        VG.new.value <- sub(pattern = '</span>', replacement = '', x = VG.new.value)
        VG.new.value <- sub(pattern = '<.*>', replacement = '', x = VG.new.value)
        VG.new.value <- sub(pattern = ',', replacement = '', x = VG.new.value)
        VG.new.value <- as.numeric(VG.new.value)

        #####################################################################################################

        DOW.chart <- unlist(append(DOW.chart, DOW.new.value))
        NASDAQ.chart <- unlist(append(NASDAQ.chart, NASDAQ.new.value))
        SP.chart <- unlist(append(SP.chart, SP.new.value))
        VG.chart <- unlist(append(VG.chart, VG.new.value))

        #####################################################################################################

        DOW.OU.value <- DOW.raw[grep(pattern = 'id-price-panel', x = DOW.raw) + 6]
        DOW.OU.value <- sub(pattern = '</span>', replacement = '', x = DOW.OU.value)
        DOW.OU.value <- sub(pattern = '<.*>', replacement = '', x = DOW.OU.value)

        NASDAQ.OU.value <- NASDAQ.raw[grep(pattern = 'id-price-panel', x = NASDAQ.raw) + 6]
        NASDAQ.OU.value <- sub(pattern = '</span>', replacement = '', x = NASDAQ.OU.value)
        NASDAQ.OU.value <- sub(pattern = '<.*>', replacement = '', x = NASDAQ.OU.value)

        SP.OU.value <- SP.raw[grep(pattern = 'id-price-panel', x = SP.raw) + 6]
        SP.OU.value <- sub(pattern = '</span>', replacement = '', x = SP.OU.value)
        SP.OU.value <- sub(pattern = '<.*>', replacement = '', x = SP.OU.value)

        VG.OU.value <- VG.raw[grep(pattern = 'id-price-panel', x = VG.raw) + 6]
        VG.OU.value <- sub(pattern = '</span>', replacement = '', x = VG.OU.value)
        VG.OU.value <- sub(pattern = '<.*>', replacement = '', x = VG.OU.value)

        #####################################################################################################

        DOW.PT.value <- DOW.raw[grep('id-price-panel', DOW.raw) + 7]
        DOW.PT.value <- sub(pattern = '</span>', replacement = '', x = DOW.PT.value)
        DOW.PT.value <- sub(pattern = '<.*>', replacement = '', x = DOW.PT.value)

        NASDAQ.PT.value <- NASDAQ.raw[grep('id-price-panel', NASDAQ.raw) + 7]
        NASDAQ.PT.value <- sub(pattern = '</span>', replacement = '', x = NASDAQ.PT.value)
        NASDAQ.PT.value <- sub(pattern = '<.*>', replacement = '', x = NASDAQ.PT.value)

        SP.PT.value <- SP.raw[grep('id-price-panel', SP.raw) + 7]
        SP.PT.value <- sub(pattern = '</span>', replacement = '', x = SP.PT.value)
        SP.PT.value <- sub(pattern = '<.*>', replacement = '', x = SP.PT.value)

        VG.PT.value <- VG.raw[grep('id-price-panel', VG.raw) + 7]
        VG.PT.value <- sub(pattern = '</span>', replacement = '', x = VG.PT.value)
        VG.PT.value <- sub(pattern = '<.*>', replacement = '', x = VG.PT.value)

        #####################################################################################################

        par(mfrow = c(2,2))

        #####################################################################################################

        plot(x = DOW.times,
            y = DOW.chart,
            type = "o",
            main = paste("LIVE DOW JONES INDUSTRIAL AVERAGE (.DJI) STREAM\n", DOW.OU.value, " ", DOW.PT.value, sep = ""),
            sub = paste("Opened at ", DOW.open.value, "  |  ", DOW.chart[length(DOW.chart)], " as of ", format(max(DOW.times), "%H:%M"), sep = ""),
            xlab = NA,
            ylab = NA)
        abline(h = max(DOW.chart), col = "green")
        abline(h = DOW.open.value, col = "gray")
        abline(h = min(DOW.chart), col = "red")

        plot(x = NASDAQ.times,
            y = NASDAQ.chart,
            type = "o",
            main = paste("LIVE NASDAQ COMPOSITE (.IXIC) STREAM\n", NASDAQ.OU.value, " ", NASDAQ.PT.value, sep = ""),
            sub = paste("Opened at ", NASDAQ.open.value, "  |  ", NASDAQ.chart[length(NASDAQ.chart)], " as of ", format(max(NASDAQ.times), "%H:%M"), sep = ""),
            xlab = NA,
            ylab = NA)
        abline(h = max(NASDAQ.chart), col = "green")
        abline(h = NASDAQ.open.value, col = "gray")
        abline(h = min(NASDAQ.chart), col = "red")

        plot(x = SP.times,
            y = SP.chart,
            type = "o",
            main = paste("LIVE S&P 500 (.INX) STREAM\n", SP.OU.value, " ", SP.PT.value, sep = ""),
            sub = paste("Opened at ", SP.open.value, "  |  ", SP.chart[length(SP.chart)], " as of ", format(max(SP.times), "%H:%M"), sep = ""),
            xlab = NA,
            ylab = NA)
        abline(h = max(SP.chart), col = "green")
        abline(h = SP.open.value, col = "gray")
        abline(h = min(SP.chart), col = "red")

        plot(x = VG.times,
            y = VG.chart,
            type = "o",
            main = paste("LIVE VANGUARD TOTAL STOCK MARKET ETF (VTI) STREAM\n", VG.OU.value, " ", VG.PT.value, sep = ""),
            sub = paste("Opened at ", VG.open.value, "  |  ", VG.chart[length(VG.chart)], " as of ", format(max(VG.times), "%H:%M"), sep = ""),
            xlab = NA,
            ylab = NA)
        abline(h = max(VG.chart), col = "green")
        abline(h = VG.open.value, col = "gray")
        abline(h = min(VG.chart), col = "red")

        #####################################################################################################

        IO <- 0

        Sys.sleep(45)

    }

}
