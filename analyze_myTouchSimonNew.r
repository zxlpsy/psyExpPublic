## ================= 参数设置 =================
## 原始数据每列含义（见 PsyToolkit 官方 touch Simon 文档）:
##  1:block名  2:congruent/incongruent(文字)  3:颜色red/green  4:一致性编码(1/2)
##  5:table row chosen  6:status(1=正确,2=按错,3=超时,此时RT=5000)  7:RT(ms)
##  8/9:触摸点X/Y坐标
expname = "myTouchSimonNew"
rtcol = 7              ## 第7列 = RT (ms)
successcol = 6         ## 第6列 = status 状态码 (1=正确, 2=按错, 3=超时)
trimSD = 3             ## 每个被试每个条件内, 剔除偏离平均值 +/- 3 SD 的试次
lowestRT = 200         ## 剔除 <200ms 的预期反应 (Simon 实验通行标准)
highestRT = 1500       ## 剔除 >1500ms 的过长反应 (超时试次 RT=5000 也一并剔除)
## --- 被试级剔除 (补充性规则, 非硬性清洗规则; 报告中需事先声明) ---
## FALSE = 不剔除任何被试, 全部纳入组统计 (默认, 与网页开关关闭时一致)
## TRUE  = 总错误率 > excludeErrThreshold% 的被试不进入组水平统计,
##         但其数据仍保留在结果表中并打上"excluded"标记 (数据透明)
excludeErrEnabled = FALSE
excludeErrThreshold = 25
followsuccess = F
conditions = c(2)      ## 按第2列 (congruent/incongruent) 分组

## 确保工作目录为本数据文件夹 (脚本需在 PsyToolkitData 文件夹内运行)
if (!file.exists(paste("exp_datafiles_", expname, ".txt", sep = ""))) {
    setwd("/Users/zhangxiaolan/Downloads/2周二午 5060 cognitive psy/PsyToolkitData_mySimon_2026_10_02_15_08")
}

## ============ RT 修剪函数 (Simon 实验标准数据清洗) ============
## 清洗顺序: 1)只保留正确试次(调用前已筛) 2)剔除 <200ms 与 >1500ms
##           3)在被试内、条件内剔除偏离均值 +/-3SD 的试次
psytkTrim2 = function(x, xsd = 3, low = NA, high = NA) {
    y = x
    if (!is.na(low))  y = y[y >= low]
    if (!is.na(high)) y = y[y <= high]
    m = mean(y)
    if (!is.na(xsd)) {
        s = sd(y) * xsd
        y = y[y > (m - s) & y < (m + s)]
    }
    return(y)
}

## ============ 逐个被试提取数据并计算 (含数据清洗) ============
data_files <- list.files("experiment_data", full.names = TRUE)
results <- data.frame()

for (f in data_files) {
    if (file.exists(f) && file.info(f)$size > 0) {
        x <- read.table(f)
        names(x) <- c("block", "condword", "color", "congruency",
                      "tablerow", "status", "RT", "touchX", "touchY")

        ## 1) 只取 test block 的正确试次 (status: 1=正确, 2=按错, 3=超时)
        x_ok <- x[x$block == "test" & x$status == 1, ]

        ## 2)+3) 条件内做 RT 清洗 (200-1500ms, +/-3SD)
        cg_rt  <- psytkTrim2(x_ok$RT[x_ok$congruency == 1], trimSD, lowestRT, highestRT)
        icg_rt <- psytkTrim2(x_ok$RT[x_ok$congruency == 2], trimSD, lowestRT, highestRT)

        ## 错误率 = (按错 + 超时) 占全部 test 试次的比例
        err_rate <- mean(x$status[x$block == "test"] != 1) * 100

        results <- rbind(results, data.frame(
            file              = basename(f),
            n_congruent       = length(cg_rt),
            n_incongruent     = length(icg_rt),
            Congruent_RT      = round(mean(cg_rt), 2),
            Incongruent_RT    = round(mean(icg_rt), 2),
            SimonEffect_ms    = round(mean(icg_rt) - mean(cg_rt), 2),
            ErrorRate_percent = round(err_rate, 1),
            excluded          = excludeErrEnabled && err_rate > excludeErrThreshold
        ))
    }
}

## 保存每个被试的结果到 CSV
write.csv(results, "myTouchSimonNew.mean.csv", row.names = FALSE)

## 打印每个被试的结果
cat("\n=============== 各被试结果 (已清洗) ===============\n")
print(results, row.names = FALSE)

## 直接在屏幕打印全组平均值 (仅用未被排除的被试)
results_included <- results[!results$excluded, ]
if (excludeErrEnabled) {
    cat("\n【被试级剔除已开启: 阈值 = 总错误率 >", excludeErrThreshold, "%】\n")
    if (any(results$excluded)) {
        cat("被排除的被试 (数据仍保留在上表中):\n")
        for (fn in results$file[results$excluded]) cat("  -", fn, "\n")
    } else {
        cat("无被试超过阈值, 全部纳入组统计.\n")
    }
}
cat("\n==========================================")
cat("\n【最终结果：请直接抄录以下数据】",
    " (组统计纳入", nrow(results_included), "/", nrow(results), "名被试)\n")
cat("Congruent (位置一致条件) 平均反应时:",
    round(mean(results_included$Congruent_RT, na.rm = TRUE), 2), "ms\n")
cat("Incongruent (位置不一致条件) 平均反应时:",
    round(mean(results_included$Incongruent_RT, na.rm = TRUE), 2), "ms\n")
cat("Simon Effect (西蒙效应量, 不一致 - 一致):",
    round(mean(results_included$SimonEffect_ms, na.rm = TRUE), 2), "ms\n")
cat("平均错误率:", round(mean(results_included$ErrorRate_percent, na.rm = TRUE), 2), "%\n")
tt <- try(t.test(results_included$Incongruent_RT, results_included$Congruent_RT,
                 paired = TRUE), silent = TRUE)
if (!inherits(tt, "try-error")) {
    cat("配对 t 检验: t(", round(tt$parameter, 2), ") = ", round(tt$statistic, 3),
        ", p = ", round(tt$p.value, 4), "\n", sep = "")
}
cat("==========================================\n")



datafilename = paste("exp_datafiles_", expname, ".txt", sep = "")
d = read.table(datafilename, as.is = T)

outputMean = NULL
outputMedian = NULL
outputMin = NULL
outputMax = NULL
outputN = NULL
outputPE = NULL

for (i in 1:length(d[, 1])) {
    if (!is.na(d[i, 1]) & file.info(d[i, 1])$size > 0) {

        print(i)

        if (exists("x")) {
            rm(x)
        }

        if (exists("exclude_lastlines")) {
            if (exclude_lastlines > 0) {
                o = system(paste("head -n", exclude_lastlines * -1, d[i, 1]), intern = T)
                x = read.table(textConnection(o), fill = T)
            }
        }

        if (exists("lastlines")) {
            if (lastlines > 0) {
                o = system(paste("tail -n", lastlines, d[i, 1]), intern = T)
                x = read.table(textConnection(o), fill = T)
            }
        }

        # if neither exclude_lastlines or lastlines has been applied
        if (!exists("x")) {
            x = read.table(d[i, 1], fill = T)
        }

        ## following block is inserted from file --------------- filter using
        ## lastlines, include_me, exclude_me

        if (exists("include_me"))
            x = x[x[, 1] == include_me, ]
        if (exists("exclude_me"))
            x = x[x[, 1] != exclude_me, ]

        ###################################################################### include
        ###################################################################### and
        ###################################################################### exclude
        ###################################################################### blocks

        ntrials = length(x[, 1])
        if (exists("blockcol")) {
            block = x[, blockcol]

            if (exists("IncludeBlocks")) {
                inclusion = block %in% IncludeBlocks
            } else {
                inclusion = rep(T, ntrials)
            }

            if (exists("ExcludeBlocks")) {
                exclusion = block %in% ExcludeBlocks
            } else {
                exclusion = rep(F, ntrials)  ## non is excluded
            }

            blockselection = inclusion & !exclusion
        } else {
            blockselection = rep(T, ntrials)
        }

        x = droplevels(x[blockselection, ])  ## droplevels removes unused levels

        ######################################################################

        sink("Rout.txt")

        ## these 4 variables are for later storage if used with multiple files,
        ## but not for single datafile analysis

        outputdataMean = numeric()
        outputdataMedian = numeric()
        outputdataMin = numeric()
        outputdataMax = numeric()
        outputdataN = numeric()
        outputdataPE = numeric()

        ###################################################################### trim
        ###################################################################### rt
        ###################################################################### data

        psytkTrim = function(x, xsd = 3, low = NA, high = NA) {
            y = x
            if (!is.na(low))
                y = y[y >= low]
            if (!is.na(high))
                y = y[y <= high]
            m = mean(y)
            if (!is.na(xsd)) {
                s = sd(y) * xsd
                y = y[y > (m - s) & y < (m + s)]
            }
            return(y)
        }

        ###################################################################### read
        ###################################################################### data

        ntrials = length(x[, 1])
        trialnum = 1:ntrials

        ## if successcol not specified all trials are success the success is
        ## stored as logical in CORRECT, and if not provided, all trials are
        ## considered to be true

        if (exists("successcol")) {
            if (exists("status_correct")) {
                CORRECT = x[, successcol] == status_correct
            } else {
                CORRECT = x[, successcol] == 1
            }
        } else {
            CORRECT = rep(T, ntrials)
        }

        ###################################################################### if
        ###################################################################### we
        ###################################################################### know
        ###################################################################### block,
        ###################################################################### code
        ###################################################################### if
        ###################################################################### a
        ###################################################################### trial
        ###################################################################### is
        ###################################################################### in
        ###################################################################### same
        ###################################################################### block
        ###################################################################### as
        ###################################################################### previous
        ###################################################################### one

        if (exists("blockcol")) {
            sameblock = c(F, x[2:ntrials, blockcol] == x[1:(ntrials - 1), blockcol])
        } else {
            sameblock = rep(T, ntrials)  ## if info is not available, always true
        }


        ###################################################################### now,
        ###################################################################### just
        ###################################################################### get
        ###################################################################### the
        ###################################################################### trial
        ###################################################################### numbers
        ###################################################################### for
        ###################################################################### each
        ###################################################################### condition
        ###################################################################### you
        ###################################################################### want
        allvalues = function(a) {
            return(a)
        }

        ## of course, it is possible there are no conditions, and all data are
        ## part of the one-and-only condition

        ## note that z is the result of by, which is a list. you can get the
        ## names of it using expand.grid.

        if (exists("conditions")) {
            z = by(trialnum, x[, conditions], allvalues)
            conditionnames = apply(expand.grid(dimnames(z)), 1, paste, collapse = "_")
        } else {
            z = list(trialnum)  ## include all trials
            conditionnames = "all_data"
        }

        ## NOTE: the tmpRtData should not be acted on with any further boolean
        ## selectors.

        for (i in 1:length(conditionnames)) {
            tc = conditionnames[i]
            selection = trialnum %in% z[[i]]

            ## if you want only trials that follow a succesful trial of course,
            ## that trial must be in the same block as previous trial as well
            if (followsuccess) {
                selection = selection & sameblock & c(F, CORRECT[1:(ntrials - 1)])
            }

            ## if necessary, trim the data based on low/high and/or SD
            if (!is.na(lowestRT) | !is.na(highestRT) | !is.na(trimSD)) {
                tmpRtData = psytkTrim(x[selection & CORRECT, rtcol], 3, lowestRT,
                  highestRT)
            } else {
                tmpRtData = x[selection & CORRECT, rtcol]
            }

            ## now report averages
            cat("Condition:", tc, "\n")
            cat("-------------------------------------------\n")
            cat("Total trials  :", sum(selection), "trials.\n")

            cat("Total correct :", sum(selection & CORRECT), "trials (errors excluded, for RT data below).\n")

            if (!is.na(lowestRT) | !is.na(highestRT) | !is.na(trimSD)) {
                cat("After RT trim :", length(tmpRtData), "\n")
            }

            cat("Error count   :", sum(!CORRECT & selection), "\n")
            cat("Mean value    :", mean(tmpRtData), "\n")
            cat("Median value  :", median(tmpRtData), "\n")
            cat("Min value     :", min(tmpRtData), "\n")
            cat("Max value     :", max(tmpRtData), "\n")
            cat("Error rate    :", (1 - sum(CORRECT[selection])/sum(selection)) *
                100, "percent\n\n")

            ## now store the data for multiple data file analysis
            if (length(outputdataMean) == 0) {
                outputdataMean = mean(tmpRtData)
            } else {
                outputdataMean = c(outputdataMean, mean(tmpRtData))
            }

            if (length(outputdataMedian) == 0) {
                outputdataMedian = median(tmpRtData)
            } else {
                outputdataMedian = c(outputdataMedian, median(tmpRtData))
            }

            if (length(outputdataMin) == 0) {
                outputdataMin = min(tmpRtData)
            } else {
                outputdataMin = c(outputdataMin, min(tmpRtData))
            }

            if (length(outputdataMax) == 0) {
                outputdataMax = max(tmpRtData)
            } else {
                outputdataMax = c(outputdataMax, max(tmpRtData))
            }

            if (length(outputdataN) == 0) {
                outputdataN = sum(selection)
            } else {
                outputdataN = c(outputdataN, sum(selection))
            }

            if (length(outputdataPE) == 0) {
                outputdataPE = (1 - sum(CORRECT[selection])/sum(selection)) * 100
            } else {
                outputdataPE = c(outputdataPE, (1 - sum(CORRECT[selection])/sum(selection)) *
                  100)
            }
        }

        # colnames( outputdata ) = conditionnames
        sink()
        ## end of block that was inserted ----------------------

        print(outputdataMean)

        outputMean = rbind(outputMean, outputdataMean)
        outputMedian = rbind(outputMedian, outputdataMedian)
        outputMin = rbind(outputMin, outputdataMin)
        outputMax = rbind(outputMax, outputdataMax)
        outputN = rbind(outputN, outputdataN)
        outputPE = rbind(outputPE, outputdataPE)
    }
}

print(outputMean)

colnames(outputMean) = conditionnames
colnames(outputMedian) = conditionnames
colnames(outputMin) = conditionnames
colnames(outputMax) = conditionnames
colnames(outputN) = conditionnames
colnames(outputPE) = conditionnames
## the problem is that not all participants will have an experiment
## datafile We know this from a NA in the datafiles file NA. This file
## just makes sure that we have output for each participant, with NA
## at the appropriate places
## This way, the files will match the main csv file

## this always creates excel files and optionally ODS files

outputMean2   = matrix(ncol=length( conditionnames ),nrow=length(d[,1]))
outputMedian2 = matrix(ncol=length( conditionnames ),nrow=length(d[,1]))
outputMin2    = matrix(ncol=length( conditionnames ),nrow=length(d[,1]))
outputMax2    = matrix(ncol=length( conditionnames ),nrow=length(d[,1]))
outputN2      = matrix(ncol=length( conditionnames ),nrow=length(d[,1]))
outputPE2     = matrix(ncol=length( conditionnames ),nrow=length(d[,1]))

colnames(outputMean2)   = conditionnames
colnames(outputMedian2) = conditionnames
colnames(outputMin2)    = conditionnames
colnames(outputMax2)    = conditionnames
colnames(outputN2)      = conditionnames
colnames(outputPE2)     = conditionnames

counter = 1
for( i in 1:length(d[,1])){
    if( is.na(d[i,1]) | file.info(d[i,1])$size == 0 ){
        outputMean2[i,]   = rep( NA , length( conditionnames ) )
        outputMedian2[i,] = rep( NA , length( conditionnames ) )
        outputMin2[i,]    = rep( NA , length( conditionnames ) )
        outputMax2[i,]    = rep( NA , length( conditionnames ) )                
        outputN2[i,]      = rep( NA , length( conditionnames ) )
        outputPE2[i,]     = rep( NA , length( conditionnames ) )        
    }else{
        outputMean2[i,]   = outputMean[counter,]
        outputMedian2[i,] = outputMedian[counter,]
        outputN2[i,]      = outputN[counter,]
        outputMin2[i,]    = outputMin[counter,]
        outputMax2[i,]    = outputMax[counter,]
        outputPE2[i,]     = outputPE[counter,]
        counter=counter+1
    }
}

## the variables outputMean2, outputMedian2 etc now contain averages for all participants
## you can write these to csv if needed with write.csv(outputMean2,"file.csv",row.names=F)
