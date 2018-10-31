;; xlsxdemo.scm
;; https://github.com/jmcnamara/xlsxwriter/Readme.md

(use-modules (ffi xlsxwriter))
(use-modules (system ffi-help-rt))

(define workbook (workbook_new "xlsxdemo.xlsx"))
(define worksheet (workbook_add_worksheet workbook NULL))

(define format (workbook_add_format workbook))

(format_set_bold format)

(worksheet_set_column worksheet 0 0 20 NULL)

(worksheet_write_string worksheet 0 0 "Hello" NULL)

(worksheet_write_string worksheet 1 0 "World" NULL)

(worksheet_write_number worksheet 2 0 123 NULL)

(worksheet_write_number worksheet 3 0 123.456 NULL)

(worksheet_insert_image worksheet 1 2 "xlsxlogo.png")

(workbook_close workbook)

;; --- last line ---
