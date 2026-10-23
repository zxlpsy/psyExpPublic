# psyExpPublic

Simon 任务（PsyToolkit 触屏版，`myTouchSimonNew`）数据分析工具集：R 脚本 + 一键分析网页。

## 文件说明

| 文件 | 用途 |
|---|---|
| `simon_analysis.html` | 一键分析网页（中英文对照）。选择 `experiment_data/` 文件夹后，自动完成清洗、统计、绘图与结果解读 |
| `simon_analysis_en.html` | 同上的**纯英文版**，供论文直接取用图注与 Results 段落 |
| `analyze_myTouchSimonNew.r` | R 脚本，实现与网页完全一致的清洗与统计规则，用于复核 |
| `visualization_comparison.r` | 不同绘图工具链的输出对比 |

## 清洗与统计规则

1. 仅使用正式实验区组（`test`）的试次；
2. 反应时只统计正确试次（状态码 1），按错（2）与超时（3，RT = 5000 ms）单独计入错误率；
3. 剔除 RT < 200 ms（预期反应）与 > 1500 ms（注意脱离）的试次；
4. 在被试内、条件内做 ±3 SD 修剪；
5. 统计量：配对样本 *t* 检验、Cohen's d<sub>z</sub>、Simon effect 的 95% CI、分条件错误率与 RT 标准差；误差线为 95% 被试间置信区间。

网页包含 8 张图：平均反应时、平均正确率、平均错误率、每被试 Simon effect、RT 分布直方图、RT 箱线图、delta plot（分布分析）、逐试次 RT 序列。

## 网页使用方法

用浏览器打开 `simon_analysis.html` 或 `simon_analysis_en.html`，点击选择（或拖入）`experiment_data` 文件夹即可。所有计算在本地浏览器完成，数据不会上传到任何服务器。

## 数据隐私

被试级数据（`experiment_data/`、`survey_data/`）、导出的 CSV / XLSX、会话日志与论文草稿均通过 `.gitignore` 排除，**不在本仓库中**。
