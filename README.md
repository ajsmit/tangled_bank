# The Tangled Bank

[![Netlify Status](https://api.netlify.com/api/v1/badges/ed1b0a51-135d-48a2-8964-0a084597291d/deploy-status)](https://app.netlify.com/sites/tangledbank/deploys)

> *"It is interesting to contemplate a tangled bank, clothed with many plants of many kinds, with birds singing on the bushes, with various insects flitting about, and with worms crawling through the damp earth, and to reflect that these elaborately constructed forms, so different from each other, and dependent upon each other in so complex a manner, have all been produced by laws acting around us."*  
> — Charles Darwin, On the Origin of Species, 1859

## 📖 Overview

The Tangled Bank is a comprehensive educational website built with [Quarto](https://quarto.org/) that serves the University of the Western Cape's Biodiversity and Conservation Biology Department. This repository contains academic materials for undergraduate and honours-level biology and statistics courses, all rich in [R](https://cran.r-project.org/) content.

**🌐 Live Website:** [tangledbank.netlify.app](http://tangledbank.netlify.app)

## 🎓 Academic Modules

### Undergraduate Courses
- **BDC223: Plant Ecophysiology** - Covers fundamental plant physiological processes, ecophysiological calculations, and environmental interactions
- **BDC334: Biogeography & Global Ecology** - Explores biodiversity patterns, macroecology, environmental gradients, and global ecological processes

### Honours Core Course  
- **BCB744: Introduction to R & Biostatistics** - Comprehensive introduction to R programming and statistical analysis for biological sciences

### Honours Elective Course
- **BCB743: Quantitative Ecology** - Advanced multivariate statistics, ordination methods, and quantitative approaches in ecological research

## 🏗️ Repository Structure

```
tangled_bank/
├── _quarto.yml                 # Main Quarto configuration
├── index.qmd                   # Website homepage
├── BDC223/                     # Plant Ecophysiology module
│   ├── L##-*.qmd              # Lecture materials
│   ├── Lab*.qmd               # Laboratory exercises
│   └── images/                # Module-specific images
├── BDC334/                     # Biogeography & Global Ecology
│   ├── Lec-##-*.qmd          # Lecture materials  
│   ├── Lab-##-*.qmd          # Laboratory exercises
│   └── assessments/          # Module assessments
├── BCB744/                     # Introduction to R & Biostatistics
│   ├── intro_r/              # R programming lessons
│   ├── basic_stats/          # Statistics lessons
│   └── assessments/          # Tests and examples
├── BCB743/                     # Quantitative Ecology
│   ├── *.qmd                 # Lesson materials
│   └── assessments/          # Module assessments
├── data/                       # Educational datasets (CSV, Excel, RData)
├── docs/                       # Academic papers and references
├── images/                     # Shared images and media
├── R/                         # Utility R functions
├── vignettes/                 # R tutorials and advanced examples
├── pages/                     # Additional content pages
├── blog/                      # Blog posts and news
└── _site/                     # Generated website content
```

## 🚀 Getting Started

### Prerequisites
- [R](https://cran.r-project.org/) (4.0+ recommended)
- [RStudio](https://rstudio.com/) (optional but recommended)  
- [Quarto](https://quarto.org/docs/get-started/) (1.3+ required)

### Installation & Setup

1. **Clone the repository:**
   ```bash
   git clone https://github.com/ajsmit/tangled_bank.git
   cd tangled_bank
   ```

2. **Open in RStudio:**
   ```r
   # Open the R project file
   # File > Open Project > tangled_bank.Rproj
   ```

3. **Install required R packages:**
   ```r
   # The website uses base R and tidyverse ecosystem primarily
   # Install packages as needed when rendering content
   ```

## 🔧 Development Workflow

### Building the Website

**Render entire website:**
```bash
quarto render
```

**Preview with live reload:**
```bash
quarto preview
```

**Render specific content:**
```bash
# Single file
quarto render path/to/file.qmd

# Entire module
quarto render BDC334/
```

**Clean build artifacts:**
```bash
quarto clean
```

### Content Development

- **Lectures:** Prefix with `Lec-` or `L##-`
- **Labs/Practicals:** Prefix with `Lab-`
- **Format:** Primarily `.qmd` (Quarto Markdown) with some legacy `.Rmd`
- **Data:** Educational datasets in `data/` directory
- **Images:** Module-specific in respective `images/` subdirectories

## 📁 Key Files & Configuration

- **`_quarto.yml`** - Main website configuration, navigation, and rendering settings
- **`tangled_bank.Rproj`** - R project configuration  
- **`styles/`** - Custom CSS styling (`styles.css`, `styles.scss`)
- **`references.bib`** - Academic references and citations
- **`.gitignore`** - Git ignore patterns for R and Quarto artifacts

## 🎨 Features

- **Responsive Design** - Mobile-friendly layout with sidebar navigation
- **Interactive Content** - Code folding, copy buttons, hover citations
- **Rich Media** - SVG figures, embedded videos, lightbox galleries  
- **Academic Tools** - Citation management, reference tooltips, hypothesis comments
- **Search** - Full-text search across all content
- **Accessibility** - Semantic HTML, keyboard navigation, screen reader support

## 📚 Content Types

### Educational Materials
- **Lectures** - Comprehensive lesson materials with theory and examples
- **Labs** - Hands-on practical exercises and tutorials
- **Assessments** - Tests, assignments, and self-evaluation tools
- **Vignettes** - Advanced R techniques and specialized analyses

### Data Resources  
- **Datasets** - Real-world biological and environmental data
- **Code Examples** - Reproducible R scripts and functions
- **Documentation** - Academic papers, references, and methodology guides

## 🤝 Contributing

This is primarily an educational repository. For issues or suggestions:

1. **Bug Reports:** Use GitHub Issues
2. **Content Suggestions:** Contact the course instructor
3. **Technical Issues:** Check Quarto documentation first

## 📄 License & Citation

- **License:** CC BY-NC-SA (Creative Commons Attribution-NonCommercial-ShareAlike)
- **Copyright:** © 2025, A.J. Smit, University of the Western Cape
- **Citation:** Please cite when using materials from this repository

## 👥 Contact & Support

**Instructor:** Professor A.J. Smit  
**Institution:** Department of Biodiversity and Conservation Biology, University of the Western Cape  
**Website:** [UWC Biodiversity & Conservation Biology](https://www.uwc.ac.za/study/all-areas-of-study/departments/department-of-biodiversity-and-conservation-biology/overview)  

## 🔗 Useful Links

- **Live Website:** [tangledbank.netlify.app](http://tangledbank.netlify.app)
- **Quarto Documentation:** [quarto.org](https://quarto.org/)
- **R Project:** [r-project.org](https://www.r-project.org/)
- **University of the Western Cape:** [uwc.ac.za](https://www.uwc.ac.za/)

---

*The Tangled Bank - Exploring the complexity and beauty of biological systems through computational methods and statistical analysis.*