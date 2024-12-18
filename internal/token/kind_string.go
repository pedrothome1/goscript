// Code generated by "stringer -type Kind"; DO NOT EDIT.

package token

import "strconv"

func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[ILLEGAL-0]
	_ = x[EOF-1]
	_ = x[IDENT-2]
	_ = x[FLOAT-3]
	_ = x[INT-4]
	_ = x[STRING-5]
	_ = x[CHAR-6]
	_ = x[BOOL-7]
	_ = x[ADD-8]
	_ = x[SUB-9]
	_ = x[MUL-10]
	_ = x[QUO-11]
	_ = x[REM-12]
	_ = x[AND-13]
	_ = x[OR-14]
	_ = x[XOR-15]
	_ = x[SHL-16]
	_ = x[SHR-17]
	_ = x[AND_NOT-18]
	_ = x[LAND-19]
	_ = x[LOR-20]
	_ = x[ARROW-21]
	_ = x[INC-22]
	_ = x[DEC-23]
	_ = x[EQL-24]
	_ = x[NEQ-25]
	_ = x[LSS-26]
	_ = x[GTR-27]
	_ = x[LEQ-28]
	_ = x[GEQ-29]
	_ = x[NOT-30]
	_ = x[ASSIGN-31]
	_ = x[DEFINE-32]
	_ = x[LPAREN-33]
	_ = x[RPAREN-34]
	_ = x[LBRACK-35]
	_ = x[RBRACK-36]
	_ = x[LBRACE-37]
	_ = x[RBRACE-38]
	_ = x[SEMICOLON-39]
	_ = x[COLON-40]
	_ = x[COMMA-41]
	_ = x[PERIOD-42]
	_ = x[ELLIPSIS-43]
	_ = x[BREAK-44]
	_ = x[CASE-45]
	_ = x[CHAN-46]
	_ = x[CONST-47]
	_ = x[CONTINUE-48]
	_ = x[DEFAULT-49]
	_ = x[DEFER-50]
	_ = x[ELSE-51]
	_ = x[FALLTHROUGH-52]
	_ = x[FOR-53]
	_ = x[FUNC-54]
	_ = x[GO-55]
	_ = x[GOTO-56]
	_ = x[IF-57]
	_ = x[IMPORT-58]
	_ = x[INTERFACE-59]
	_ = x[MAP-60]
	_ = x[RANGE-61]
	_ = x[RETURN-62]
	_ = x[STRUCT-63]
	_ = x[SWITCH-64]
	_ = x[TYPE-65]
	_ = x[VAR-66]
	_ = x[PRINT-67]
	_ = x[NIL-68]
}

const _Kind_name = "ILLEGALEOFIDENTFLOATINTSTRINGCHARBOOLADDSUBMULQUOREMANDORXORSHLSHRAND_NOTLANDLORARROWINCDECEQLNEQLSSGTRLEQGEQNOTASSIGNDEFINELPARENRPARENLBRACKRBRACKLBRACERBRACESEMICOLONCOLONCOMMAPERIODELLIPSISBREAKCASECHANCONSTCONTINUEDEFAULTDEFERELSEFALLTHROUGHFORFUNCGOGOTOIFIMPORTINTERFACEMAPRANGERETURNSTRUCTSWITCHTYPEVARPRINTNIL"

var _Kind_index = [...]uint16{0, 7, 10, 15, 20, 23, 29, 33, 37, 40, 43, 46, 49, 52, 55, 57, 60, 63, 66, 73, 77, 80, 85, 88, 91, 94, 97, 100, 103, 106, 109, 112, 118, 124, 130, 136, 142, 148, 154, 160, 169, 174, 179, 185, 193, 198, 202, 206, 211, 219, 226, 231, 235, 246, 249, 253, 255, 259, 261, 267, 276, 279, 284, 290, 296, 302, 306, 309, 314, 317}

func (i Kind) String() string {
	if i < 0 || i >= Kind(len(_Kind_index)-1) {
		return "Kind(" + strconv.FormatInt(int64(i), 10) + ")"
	}
	return _Kind_name[_Kind_index[i]:_Kind_index[i+1]]
}
