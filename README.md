# IVT Optimizer  
> *Personalized Intravenous Thrombolysis Decision Support for Acute Ischemic Stroke*

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE.md)  
[![Platform](https://img.shields.io/badge/Platform-Shiny-green.svg)](https://www.shinyapps.io/)  
[![Status](https://img.shields.io/badge/Status-Production_Ready-brightgreen.svg)](https://kwithna.shinyapps.io/IVT_Optimizer/)  

---

## 📌 Overview 
**IVT Optimizer** is an interactive platform for personalized intravenous thrombolysis (IVT) treatment selection based on individual patient characteristics and clinical settings for patients with acute ischemic stroke (AIS).

## 🔬 Evidence Base  
The platform integrates results from a **network meta-analysis (NMA)** and **multidimensional benchmarking study** from published phase 3 randomized controlled trials, evaluating treatments across **four key domains**:
1. **Safety** – Serious adverse events, intracranial hemorrhage, other endpoints, and numbers of serious adverse events classified into the eight major systems
2. **Efficacy** – Efficacy outcomes evaluated by modified Rankin Scale, National Institute of Health stroke scale, and Barthel index
3. **Convenience** – Administration complexity, storage requirements
4. **Affordability** – Treatment cost

In default mode， **four key domains** are quantified and weighted to compute an overall score:
| Domain | Weight (default) | Method |
|--------|------------------|--------|
| Safety | 5 | P-scores from network meta-analysis |
| Efficacy | 5 | P-scores from network meta-analysis |
| Convenience | 0.5 | 3-tier scoring (storage 30 % + administration 70 %) |
| Affordability | 0.5 | Cost difference vs. highest-cost treatment (USD, 60 kg patient) |

Users can **dynamically adjust domain weights**, select therapeutic windows (≤4.5 h vs >4.5 h), and customize local cost/convenience parameters to obtain **personalized treatment rankings** in real time.

## 🚀 How to Use  
1. **Access**  
- Web: [https://kwithna.shinyapps.io/IVT_Optimizer/](https://kwithna.shinyapps.io/IVT_Optimizer/)  

2. **Set Weights**  
- Drag sliders in the **Weight Adjustment** panel to reflect patient priorities.

3. **Select Therapeutic Window**  
- **ALL**: No restriction
- **IN**: ≤ 4.5 h
- **OUT**: > 4.5 h  

5. **Customize Advanced Parameters**  
- **Storage Convenience**: Modify temperature, light, and shelf-life grading.  
- **Drug Costs**: Enter local prices (USD) or post-reimbursement values.

6. **Review Results**  
- Treatments are ranked by **overall score**; click column headers to sort them by individual domains.

## 🛠️ Local Deployment
### Prerequisites
- R ≥ 4.4.0
- R studio ≥ 2023.06.1 Build 524
- shiny-package ≥ 1.10.0
### Quick Start
#### 1. Download the Package
- Click the green **Code** button above ➜ **Download ZIP**.
- Unzip the file to a local folder.

#### 2. Set Up & Run
- Open R and **set working directory** to the unzipped folder.
```r
# For example:
setwd("D:/IVT_Optimizer/Script")
```
- Run codes in `IVT_Optimizer.R` and launch the app.

## 📜 Citation
Not currently available.

## 🤝 Contributing 
Pull requests are welcome for:  
- Bug fixes  
- Additional treatments / outcomes  
- Localization / translations  
- UI/UX improvements

## 📞 Contact
- Lead Developer: Dr. Xuefan Yao (kwithna.y@gmail.com)
- Corresponding PI: Dr. Haiqing Song (songhq@xwhosp.org)
- Institution: Department of Neurology, Xuanwu Hospital, Capital Medical University, Beijing, China

## 🙏 Acknowledgments
**Funders** 
- Science and Technology Innovation 2030-Major Project (No. 2021ZD0201806)
- National Key R&D Program of China (No. 2022YFC3600504)
- Beijing Nova Program (No. 20240484612)
  
**Inspiration** 
- The *dynbenchmark* project ([https://github.com/dynverse/dynbenchmark](https://github.com/dynverse/dynbenchmark)) by Saelens et al. provided critical methodological insights for our multidimensional benchmarking. Their open-source framework for comparing single-cell trajectory inference methods served as a foundational blueprint for this work.

**Thanks**
- We would like to thank all the researchers and clinicians from original studies contributing to our works.


IVT Optimizer – Precision in intravenous thrombolysis, personalized for every stroke patient.
