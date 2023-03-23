#' Examination for the Certificate of Proficiency in English (ECPE)
#'
#' This is data from the grammar section of the ECPE, administered annually by
#' the English Language Institute at the University of Michigan. This data
#' contains responses to 28 questions from 2,922 respondents, which ask
#' respondents to complete a sentence with the correct word. This data set has
#' been used by Templin & Hoffman (2013) and Templin & Bradshaw (2014) for
#' demonstrating the log-linear cognitive diagnosis model (LCDM) and the
#' hierarchical diagnostic classification model (HDCM), respectively.
#'
#' @details
#' The skills correspond to knowledge of:
#' 1. Morphosyntactic rules
#' 2. Cohesive rules
#' 3. Lexical rules
#'
#' For more details, see Buck & Tatsuoka (1998) and Henson & Templin (2007).
#'
#' @format `ecpe_data` is a [tibble][tibble::tibble-package] containing ECPE
#' response data with 2,922 rows and 29 variables.
#' * `resp_id`: Respondent identifier
#' * `E1`-`E28`: Dichotomous item responses to the 28 ECPE items
#'
#' @references Buck, G., & Tatsuoka, K. K. (1998). Application of the rule-space
#'   procedure to language testing: Examining attributes of a free response
#'   listening test. *Language Testing, 15*(2), 119-157.
#'   \doi{10.1177/026553229801500201}
#' @references Henson, R., & Templin, J. (2007, April). *Large-scale language
#'   assessment using cognitive diagnosis models.* Paper presented at the Annual
#'   meeting of the National Council on Measurement in Education, Chicago, IL.
#' @references Templin, J., & Hoffman, L. (2013). Obtaining diagnostic
#'   classification model estimates using Mplus. *Educational Measurement:
#'   Issues and Practice, 32*(2), 37-50.
#'   \doi{10.1111/emip.12010}
#' @references Templin, J., & Bradshaw, L. (2014). Hierarchical diagnostic
#'   classification models: A family of models for estimating and testing
#'   attribute hierarchies. *Psychometrika, 79*(2), 317-339.
#'   \doi{10.1007/s11336-013-9362-0}
#' @rdname ecpe
"ecpe_data"

#' @format `ecpe_qmatrix` is a [tibble][tibble::tibble-package] that identifies
#' which skills are measured by each ECPE item. This section of the ECPE
#' contains 28 items measuring 3 skills. The `ecpe_qmatrix` correspondingly is
#' made up of 28 rows and 4 variables.
#' * `item_id`: Item identifier, corresponds to `E1`-`E28` in [`ecpe_data`]
#' * `morphosyntactic`, `cohesive`, and `lexical`: Dichotomous indicator for
#'   whether or not the skill is measured by each item. A value of `1` indicates
#'   the skill is measured by the item and a value of `0` indicates the skill is
#'   not measured by the item.
#' @rdname ecpe
"ecpe_qmatrix"


#' MacReady & Dayton (1977) Multiplication Data
#'
#' This is a small data set of multiplication item responses. This data contains
#' responses to 4 items from 142 respondents, which ask respondents to complete
#' an integer multiplication problem.
#'
#' @format `mdm_data` is a [tibble][tibble::tibble-package] containing responses
#' to multiplication items, as described in MacReady & Dayton (1977). There are
#' 142 rows and 5 variables.
#' * `respondent`: Respondent identifier
#' * `mdm1`-`mdm4`: Dichotomous item responses to the 4 multiplication items
#'
#' @references MacReady, G. B., & Dayton, C. M. (1977). The use of probabilistic
#'   models in the assessment of mastery. *Journal of Educational Statistics,
#'   2*(2), 99-120. \doi{10.2307/1164802}
#' @rdname mdm
"mdm_data"

#' @format `mdm_qmatrix` is a [tibble][tibble::tibble-package] that identifies
#' which skills are measured by each MDM item. This MDM data contains 4 items,
#' all of which measure the skill of multiplication. The `mdm_qmatrix`
#' correspondingly is made up of 4 rows and 2 variables.
#' * `item`: Item identifier, corresponds to `mdm1`-`mdm4` in [`mdm_data`]
#' * `multiplication`: Dichotomous indicator for whether or not the
#'   multiplication skill is measured by each item. A value of `1` indicates the
#'   skill is measured by the item and a value of `0` indicates the skill is not
#'   measured by the item.
#' @rdname mdm
"mdm_qmatrix"
