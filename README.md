# rdp2
## R Data Processing

`rdp2` is my pet project library for R. It is designed to perform the most common tasks in data processing for marketing, sociological, psychological, and other surveys. This library offers not just a set of functions for working with traditional data frames but rather a whole new approach for working with datasets, making it closer to a framework than a library.

### User Manual
For detailed guidance on using `rdp2`, please refer to the [rdp2 user manual](https://kristensen-kor.quarto.pub/rdp2-book/). This comprehensive guide covers the core principles, functionalities, and practical applications of the package to enhance your data processing workflows.

### Core Principles
The core principles around which this library is built:
* Categorical variables are labeled numeric vectors, not factors.
* Native support for multiple-response questions, with workflows for these variables being the same as for single-categorical variables.
* Support for working with weighted data.
* Unlike traditional R's immutable approach to datasets, this library is built upon R6 classes, allowing for mutable operations. Although this changes the traditional approach, the resulting workflow feels more natural and convenient.

### Main Functionality
* Creation of variables and recodings (means, T2B, NPS, etc.).
* Import of SPSS datasets.
* Restructuring of datasets.
* Creation of Excel tables with formatting and significance checks.

### Development Status
The project is still in the early stages of development, and breaking changes are possible. While the core functionality is established, new features and improvements are ongoing. Now, with the [rdp2 user manual](https://kristensen-kor.quarto.pub/rdp2-book/) available, users have access to comprehensive documentation to support their work with the package. Although comprehensive R help files for framework methods are not yet available, this is due to the limited support R provides for documenting R6 classes.

### Learning and Contributions
I am still learning R, GitHub, and open source practices, so some aspects of this project may be questionable or experimental.

### Why rdp2? What happened to rdp1?
Yes, there was another version - `rdp`. It never made it to the public. It relied on tibbles and variable attributes to store metadata (such as variable labels and value labels), but it was very unreliable. Many operations in R tend to strip attributes, making it impractical to use since it required additional treatment to preserve those attributes. Additionally, reassigning the dataset to itself after each operation due to its immutability was very bothersome and inconvenient, as literally every operation during data processing required such reassignment. That’s why it was rewritten from scratch using R6 classes, which utilize separate data structures for metadata while preserving the main data object as a usual data.frame (tibble). This approach is more intuitive for working with regular dplyr functions.

### Dependencies
* `tidyverse` - `rdp2` primarily relies on tibbles and dplyr functions at its core.
* `openxlsx` - Used for exporting to Excel.
* `haven` - Used for importing/exporting SPSS .sav datasets.
* `R6`

### Contributing

At this time, I am not accepting any pull requests or contributions. The project is still in the very early stages of development, and my vision for it extends beyond what is currently presented on GitHub. As a result, a lot may change, including core functions.

