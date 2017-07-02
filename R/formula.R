#' Insert formula for created variables used in PROMISE.
#'
#' @param variable The variable's formula to output.
#'
#' @return Outputs a string of the LaTeX math equation.
#'
#' @export
insert_formula <-
    function(variable = c("eGFR_f1", "eGFR_f2", "eGFR_m1", "eGFR_m2",
                          "HOMA", "ISI", "IGIIR", "ISSI2")) {
        variable <- match.arg(variable)

        equation <- switch(
            variable,
            eGFR_f1 = paste(
                "$$144 \\times (\\text{Scr} / 0.7)^{-0.329} \\times 0.993^{\\text{Age}}[\\times\\text{1.159 if black}]$$"
            ),
            eGFR_f2 = paste(
                "$$144 \\times (\\text{Scr} / 0.7)^{-1.209} \\times 0.993^{\\text{Age}}[\\times\\text{1.159 if black}]$$"
            ),
            eGFR_m1 = paste(
                "$$141 \\times (\\text{Scr} / 0.7)^{-0.411} \\times 0.993^{\\text{Age}}[\\times\\text{1.159 if black}]$$"
            ),
            eGFR_m2 = paste(
                "$$141 \\times (\\text{Scr} / 0.7)^{-1.209} \\times 0.993^{\\text{Age}}[\\times\\text{1.159 if black}]$$"
            ),
            HOMA = paste(
                "$$\\text{HOMA-IR} = \\frac{\\mathrm{G_{0min}} \\times \\mathrm{I_{0min}}}{22.5}$$"
            ),
            ISI = paste(
                "$$\\text{ISI}_{\\text{OGTT}} = \\frac{10000}{\\sqrt{(\\mathrm{G_{0min}} \\times \\mathrm{I_{0min}}) \\times (\\mathrm{G_{mean}} \\times \\mathrm{I_{mean}} )}}$$"
            ),
            IGIIR = paste(
                "$$\\text{IGI/IR} = \\frac{\\frac{\\mathrm{I_{30min}} - \\mathrm{I_{0min}}}{\\mathrm{G_{30min}} - \\mathrm{G_{0min}}}}{\\text{HOMA-IR}}$$"
            ),
            ISSI2 = paste(
                "$$\\text{ISSI-2} = \\left(\\frac{\\mathrm{Insulin\\: AUC}}{\\mathrm{Glucose\\: AUC}}\\right) \\times \\mathrm{ISI}$$"
            )
        )

        cat(equation)
    }
