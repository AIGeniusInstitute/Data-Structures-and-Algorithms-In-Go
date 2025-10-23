package Data_Structures_and_Algorithms_In_Go

import (
	"fmt"
	"io"
	"log"
	"reflect"
	"sort"
	"strconv"
	"text/tabwriter"
)

func nonzero(v reflect.Value) bool {
	switch v.Kind() {
	case reflect.Bool:
		return v.Bool()
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return v.Int() != 0
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		return v.Uint() != 0
	case reflect.Float32, reflect.Float64:
		return v.Float() != 0
	case reflect.Complex64, reflect.Complex128:
		return v.Complex() != complex(0, 0)
	case reflect.String:
		return v.String() != ""
	case reflect.Struct:
		for i := 0; i < v.NumField(); i++ {
			if nonzero(getField(v, i)) {
				return true
			}
		}
		return false
	case reflect.Array:
		for i := 0; i < v.Len(); i++ {
			if nonzero(v.Index(i)) {
				return true
			}
		}
		return false
	case reflect.Map, reflect.Interface, reflect.Slice, reflect.Ptr, reflect.Chan, reflect.Func:
		return !v.IsNil()
	case reflect.UnsafePointer:
		return v.Pointer() != 0
	}
	return true
}

// Errorf is a convenience wrapper for fmt.Errorf.
//
// Calling Errorf(f, x, y) is equivalent to
// fmt.Errorf(f, Formatter(x), Formatter(y)).
func Errorf(format string, a ...interface{}) error {
	return fmt.Errorf(format, wrap(a, false)...)
}

// Fprintf is a convenience wrapper for fmt.Fprintf.
//
// Calling Fprintf(w, f, x, y) is equivalent to
// fmt.Fprintf(w, f, Formatter(x), Formatter(y)).
func Fprintf(w io.Writer, format string, a ...interface{}) (n int, error error) {
	return fmt.Fprintf(w, format, wrap(a, false)...)
}

// Log is a convenience wrapper for log.Printf.
//
// Calling Log(x, y) is equivalent to
// log.Print(Formatter(x), Formatter(y)), but each operand is
// formatted with "%# v".
func Log(a ...interface{}) {
	log.Print(wrap(a, true)...)
}

// Logf is a convenience wrapper for log.Printf.
//
// Calling Logf(f, x, y) is equivalent to
// log.Printf(f, Formatter(x), Formatter(y)).
func Logf(format string, a ...interface{}) {
	log.Printf(format, wrap(a, false)...)
}

// Logln is a convenience wrapper for log.Printf.
//
// Calling Logln(x, y) is equivalent to
// log.Println(Formatter(x), Formatter(y)), but each operand is
// formatted with "%# v".
func Logln(a ...interface{}) {
	log.Println(wrap(a, true)...)
}

// Print pretty-prints its operands and writes to standard output.
//
// Calling Print(x, y) is equivalent to
// fmt.Print(Formatter(x), Formatter(y)), but each operand is
// formatted with "%# v".
func Print(a ...interface{}) (n int, errno error) {
	return fmt.Print(wrap(a, true)...)
}

// Printf is a convenience wrapper for fmt.Printf.
//
// Calling Printf(f, x, y) is equivalent to
// fmt.Printf(f, Formatter(x), Formatter(y)).
func Printf(format string, a ...interface{}) (n int, errno error) {
	return fmt.Printf(format, wrap(a, false)...)
}

// Println pretty-prints its operands and writes to standard output.
//
// Calling Println(x, y) is equivalent to
// fmt.Println(Formatter(x), Formatter(y)), but each operand is
// formatted with "%# v".
func Println(a ...interface{}) (n int, errno error) {
	return fmt.Println(wrap(a, true)...)
}

// Sprint is a convenience wrapper for fmt.Sprintf.
//
// Calling Sprint(x, y) is equivalent to
// fmt.Sprint(Formatter(x), Formatter(y)), but each operand is
// formatted with "%# v".
func Sprint(a ...interface{}) string {
	return fmt.Sprint(wrap(a, true)...)
}

// Sprintf is a convenience wrapper for fmt.Sprintf.
//
// Calling Sprintf(f, x, y) is equivalent to
// fmt.Sprintf(f, Formatter(x), Formatter(y)).
func Sprintf(format string, a ...interface{}) string {
	return fmt.Sprintf(format, wrap(a, false)...)
}

func wrap(a []interface{}, force bool) []interface{} {
	w := make([]interface{}, len(a))
	for i, x := range a {
		w[i] = formatter{v: reflect.ValueOf(x), force: force}
	}
	return w
}

type formatter struct {
	v     reflect.Value
	force bool
	quote bool
}

// Formatter makes a wrapper, f, that will format x as go source with line
// breaks and tabs. Object f responds to the "%v" formatting verb when both the
// "#" and " " (space) flags are set, for example:
//
//	fmt.Sprintf("%# v", Formatter(x))
//
// If one of these two flags is not set, or any other verb is used, f will
// format x according to the usual rules of package fmt.
// In particular, if x satisfies fmt.Formatter, then x.Format will be called.
func Formatter(x interface{}) (f fmt.Formatter) {
	return formatter{v: reflect.ValueOf(x), quote: true}
}

func (fo formatter) String() string {
	return fmt.Sprint(fo.v.Interface()) // unwrap it
}

func (fo formatter) passThrough(f fmt.State, c rune) {
	s := "%"
	for i := 0; i < 128; i++ {
		if f.Flag(i) {
			s += string(rune(i))
		}
	}
	if w, ok := f.Width(); ok {
		s += fmt.Sprintf("%d", w)
	}
	if p, ok := f.Precision(); ok {
		s += fmt.Sprintf(".%d", p)
	}
	s += string(c)
	fmt.Fprintf(f, s, fo.v.Interface())
}

func (fo formatter) Format(f fmt.State, c rune) {
	if fo.force || c == 'v' && f.Flag('#') && f.Flag(' ') {
		w := tabwriter.NewWriter(f, 4, 4, 1, ' ', 0)
		p := &printer{tw: w, Writer: w, visited: make(map[visit]int)}
		p.printValue(fo.v, true, fo.quote)
		w.Flush()
		return
	}
	fo.passThrough(f, c)
}

type printer struct {
	io.Writer
	tw      *tabwriter.Writer
	visited map[visit]int
	depth   int
}

// Indent inserts prefix at the beginning of each non-empty line of s. The
// end-of-line marker is NL.
func Indent(s, prefix string) string {
	return string(IndentBytes([]byte(s), []byte(prefix)))
}

// IndentBytes inserts prefix at the beginning of each non-empty line of b.
// The end-of-line marker is NL.
func IndentBytes(b, prefix []byte) []byte {
	var res []byte
	bol := true
	for _, c := range b {
		if bol && c != '\n' {
			res = append(res, prefix...)
		}
		res = append(res, c)
		bol = c == '\n'
	}
	return res
}

// Writer indents each line of its input.
type indentWriter struct {
	w   io.Writer
	bol bool
	pre [][]byte
	sel int
	off int
}

// NewIndentWriter makes a new write filter that indents the input
// lines. Each line is prefixed in order with the corresponding
// element of pre. If there are more lines than elements, the last
// element of pre is repeated for each subsequent line.
func NewIndentWriter(w io.Writer, pre ...[]byte) io.Writer {
	return &indentWriter{
		w:   w,
		pre: pre,
		bol: true,
	}
}

// The only errors returned are from the underlying indentWriter.
func (w *indentWriter) Write(p []byte) (n int, err error) {
	for _, c := range p {
		if w.bol {
			var i int
			i, err = w.w.Write(w.pre[w.sel][w.off:])
			w.off += i
			if err != nil {
				return n, err
			}
		}
		_, err = w.w.Write([]byte{c})
		if err != nil {
			return n, err
		}
		n++
		w.bol = c == '\n'
		if w.bol {
			w.off = 0
			if w.sel < len(w.pre)-1 {
				w.sel++
			}
		}
	}
	return n, nil
}

func (p *printer) indent() *printer {
	q := *p
	q.tw = tabwriter.NewWriter(p.Writer, 4, 4, 1, ' ', 0)
	q.Writer = NewIndentWriter(q.tw, []byte{'\t'})
	return &q
}

func (p *printer) printInline(v reflect.Value, x interface{}, showType bool) {
	if showType {
		io.WriteString(p, v.Type().String())
		fmt.Fprintf(p, "(%#v)", x)
	} else {
		fmt.Fprintf(p, "%#v", x)
	}
}

// printValue must keep track of already-printed pointer values to avoid
// infinite recursion.
type visit struct {
	v   uintptr
	typ reflect.Type
}

func (p *printer) catchPanic(v reflect.Value, method string) {
	if r := recover(); r != nil {
		if v.Kind() == reflect.Ptr && v.IsNil() {
			writeByte(p, '(')
			io.WriteString(p, v.Type().String())
			io.WriteString(p, ")(nil)")
			return
		}
		writeByte(p, '(')
		io.WriteString(p, v.Type().String())
		io.WriteString(p, ")(PANIC=calling method ")
		io.WriteString(p, strconv.Quote(method))
		io.WriteString(p, ": ")
		fmt.Fprint(p, r)
		writeByte(p, ')')
	}
}

func (p *printer) printValue(v reflect.Value, showType, quote bool) {
	if p.depth > 10 {
		io.WriteString(p, "!%v(DEPTH EXCEEDED)")
		return
	}

	if v.IsValid() && v.CanInterface() {
		i := v.Interface()
		if goStringer, ok := i.(fmt.GoStringer); ok {
			defer p.catchPanic(v, "GoString")
			io.WriteString(p, goStringer.GoString())
			return
		}
	}

	switch v.Kind() {
	case reflect.Bool:
		p.printInline(v, v.Bool(), showType)
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		p.printInline(v, v.Int(), showType)
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		p.printInline(v, v.Uint(), showType)
	case reflect.Float32, reflect.Float64:
		p.printInline(v, v.Float(), showType)
	case reflect.Complex64, reflect.Complex128:
		fmt.Fprintf(p, "%#v", v.Complex())
	case reflect.String:
		p.fmtString(v.String(), quote)
	case reflect.Map:
		t := v.Type()
		if showType {
			io.WriteString(p, t.String())
		}
		writeByte(p, '{')
		if nonzero(v) {
			expand := !canInline(v.Type())
			pp := p
			if expand {
				writeByte(p, '\n')
				pp = p.indent()
			}
			sm := Sort(v)
			for i := 0; i < v.Len(); i++ {
				k := sm.Key[i]
				mv := sm.Value[i]
				pp.printValue(k, false, true)
				writeByte(pp, ':')
				if expand {
					writeByte(pp, '\t')
				}
				showTypeInStruct := t.Elem().Kind() == reflect.Interface
				pp.printValue(mv, showTypeInStruct, true)
				if expand {
					io.WriteString(pp, ",\n")
				} else if i < v.Len()-1 {
					io.WriteString(pp, ", ")
				}
			}
			if expand {
				pp.tw.Flush()
			}
		}
		writeByte(p, '}')
	case reflect.Struct:
		t := v.Type()
		if v.CanAddr() {
			addr := v.UnsafeAddr()
			vis := visit{addr, t}
			if vd, ok := p.visited[vis]; ok && vd < p.depth {
				p.fmtString(t.String()+"{(CYCLIC REFERENCE)}", false)
				break // don't print v again
			}
			p.visited[vis] = p.depth
		}

		if showType {
			io.WriteString(p, t.String())
		}
		writeByte(p, '{')
		if nonzero(v) {
			expand := !canInline(v.Type())
			pp := p
			if expand {
				writeByte(p, '\n')
				pp = p.indent()
			}
			for i := 0; i < v.NumField(); i++ {
				showTypeInStruct := true
				if f := t.Field(i); f.Name != "" {
					io.WriteString(pp, f.Name)
					writeByte(pp, ':')
					if expand {
						writeByte(pp, '\t')
					}
					showTypeInStruct = labelType(f.Type)
				}
				pp.printValue(getField(v, i), showTypeInStruct, true)
				if expand {
					io.WriteString(pp, ",\n")
				} else if i < v.NumField()-1 {
					io.WriteString(pp, ", ")
				}
			}
			if expand {
				pp.tw.Flush()
			}
		}
		writeByte(p, '}')
	case reflect.Interface:
		switch e := v.Elem(); {
		case e.Kind() == reflect.Invalid:
			io.WriteString(p, "nil")
		case e.IsValid():
			pp := *p
			pp.depth++
			pp.printValue(e, showType, true)
		default:
			io.WriteString(p, v.Type().String())
			io.WriteString(p, "(nil)")
		}
	case reflect.Array, reflect.Slice:
		t := v.Type()
		if showType {
			io.WriteString(p, t.String())
		}
		if v.Kind() == reflect.Slice && v.IsNil() && showType {
			io.WriteString(p, "(nil)")
			break
		}
		if v.Kind() == reflect.Slice && v.IsNil() {
			io.WriteString(p, "nil")
			break
		}
		writeByte(p, '{')
		expand := !canInline(v.Type())
		pp := p
		if expand {
			writeByte(p, '\n')
			pp = p.indent()
		}
		for i := 0; i < v.Len(); i++ {
			showTypeInSlice := t.Elem().Kind() == reflect.Interface
			pp.printValue(v.Index(i), showTypeInSlice, true)
			if expand {
				io.WriteString(pp, ",\n")
			} else if i < v.Len()-1 {
				io.WriteString(pp, ", ")
			}
		}
		if expand {
			pp.tw.Flush()
		}
		writeByte(p, '}')
	case reflect.Ptr:
		e := v.Elem()
		if !e.IsValid() {
			writeByte(p, '(')
			io.WriteString(p, v.Type().String())
			io.WriteString(p, ")(nil)")
		} else {
			pp := *p
			pp.depth++
			writeByte(pp, '&')
			pp.printValue(e, true, true)
		}
	case reflect.Chan:
		x := v.Pointer()
		if showType {
			writeByte(p, '(')
			io.WriteString(p, v.Type().String())
			fmt.Fprintf(p, ")(%#v)", x)
		} else {
			fmt.Fprintf(p, "%#v", x)
		}
	case reflect.Func:
		io.WriteString(p, v.Type().String())
		io.WriteString(p, " {...}")
	case reflect.UnsafePointer:
		p.printInline(v, v.Pointer(), showType)
	case reflect.Invalid:
		io.WriteString(p, "nil")
	}
}

func canInline(t reflect.Type) bool {
	switch t.Kind() {
	case reflect.Map:
		return !canExpand(t.Elem())
	case reflect.Struct:
		for i := 0; i < t.NumField(); i++ {
			if canExpand(t.Field(i).Type) {
				return false
			}
		}
		return true
	case reflect.Interface:
		return false
	case reflect.Array, reflect.Slice:
		return !canExpand(t.Elem())
	case reflect.Ptr:
		return false
	case reflect.Chan, reflect.Func, reflect.UnsafePointer:
		return false
	}
	return true
}

func canExpand(t reflect.Type) bool {
	switch t.Kind() {
	case reflect.Map, reflect.Struct,
		reflect.Interface, reflect.Array, reflect.Slice,
		reflect.Ptr:
		return true
	}
	return false
}

func labelType(t reflect.Type) bool {
	switch t.Kind() {
	case reflect.Interface, reflect.Struct:
		return true
	}
	return false
}

func (p *printer) fmtString(s string, quote bool) {
	if quote {
		s = strconv.Quote(s)
	}
	io.WriteString(p, s)
}

func writeByte(w io.Writer, b byte) {
	w.Write([]byte{b})
}

func getField(v reflect.Value, i int) reflect.Value {
	val := v.Field(i)
	if val.Kind() == reflect.Interface && !val.IsNil() {
		val = val.Elem()
	}
	return val
}

// Note: Throughout this package we avoid calling reflect.Value.Interface as
// it is not always legal to do so and it's easier to avoid the issue than to face it.

// SortedMap represents a map's keys and values. The keys and values are
// aligned in index order: Value[i] is the value in the map corresponding to Key[i].
type SortedMap struct {
	Key   []reflect.Value
	Value []reflect.Value
}

func (o *SortedMap) Len() int           { return len(o.Key) }
func (o *SortedMap) Less(i, j int) bool { return compare(o.Key[i], o.Key[j]) < 0 }
func (o *SortedMap) Swap(i, j int) {
	o.Key[i], o.Key[j] = o.Key[j], o.Key[i]
	o.Value[i], o.Value[j] = o.Value[j], o.Value[i]
}

const brokenNaNs = false

func mapElems(mapValue reflect.Value) ([]reflect.Value, []reflect.Value) {
	// Note: this code is arranged to not panic even in the presence
	// of a concurrent map update. The runtime is responsible for
	// yelling loudly if that happens. See issue 33275.
	n := mapValue.Len()
	key := make([]reflect.Value, 0, n)
	value := make([]reflect.Value, 0, n)
	iter := mapValue.MapRange()
	for iter.Next() {
		key = append(key, iter.Key())
		value = append(value, iter.Value())
	}
	return key, value
}

// Sort accepts a map and returns a SortedMap that has the same keys and
// values but in a stable sorted order according to the keys, modulo issues
// raised by unorderable key values such as NaNs.
//
// The ordering rules are more general than with Go's < operator:
//
//   - when applicable, nil compares low
//   - ints, floats, and strings order by <
//   - NaN compares less than non-NaN floats
//   - bool compares false before true
//   - complex compares real, then imag
//   - pointers compare by machine address
//   - channel values compare by machine address
//   - structs compare each field in turn
//   - arrays compare each element in turn.
//     Otherwise identical arrays compare by length.
//   - interface values compare first by reflect.Type describing the concrete type
//     and then by concrete value as described in the previous rules.
func Sort(mapValue reflect.Value) *SortedMap {
	if mapValue.Type().Kind() != reflect.Map {
		return nil
	}
	key, value := mapElems(mapValue)
	sorted := &SortedMap{
		Key:   key,
		Value: value,
	}
	sort.Stable(sorted)
	return sorted
}

// compare compares two values of the same type. It returns -1, 0, 1
// according to whether a > b (1), a == b (0), or a < b (-1).
// If the types differ, it returns -1.
// See the comment on Sort for the comparison rules.
func compare(aVal, bVal reflect.Value) int {
	aType, bType := aVal.Type(), bVal.Type()
	if aType != bType {
		return -1 // No good answer possible, but don't return 0: they're not equal.
	}
	switch aVal.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		a, b := aVal.Int(), bVal.Int()
		switch {
		case a < b:
			return -1
		case a > b:
			return 1
		default:
			return 0
		}
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		a, b := aVal.Uint(), bVal.Uint()
		switch {
		case a < b:
			return -1
		case a > b:
			return 1
		default:
			return 0
		}
	case reflect.String:
		a, b := aVal.String(), bVal.String()
		switch {
		case a < b:
			return -1
		case a > b:
			return 1
		default:
			return 0
		}
	case reflect.Float32, reflect.Float64:
		return floatCompare(aVal.Float(), bVal.Float())
	case reflect.Complex64, reflect.Complex128:
		a, b := aVal.Complex(), bVal.Complex()
		if c := floatCompare(real(a), real(b)); c != 0 {
			return c
		}
		return floatCompare(imag(a), imag(b))
	case reflect.Bool:
		a, b := aVal.Bool(), bVal.Bool()
		switch {
		case a == b:
			return 0
		case a:
			return 1
		default:
			return -1
		}
	case reflect.Ptr:
		a, b := aVal.Pointer(), bVal.Pointer()
		switch {
		case a < b:
			return -1
		case a > b:
			return 1
		default:
			return 0
		}
	case reflect.Chan:
		if c, ok := nilCompare(aVal, bVal); ok {
			return c
		}
		ap, bp := aVal.Pointer(), bVal.Pointer()
		switch {
		case ap < bp:
			return -1
		case ap > bp:
			return 1
		default:
			return 0
		}
	case reflect.Struct:
		for i := range aVal.NumField() {
			if c := compare(aVal.Field(i), bVal.Field(i)); c != 0 {
				return c
			}
		}
		return 0
	case reflect.Array:
		for i := range aVal.Len() {
			if c := compare(aVal.Index(i), bVal.Index(i)); c != 0 {
				return c
			}
		}
		return 0
	case reflect.Interface:
		if c, ok := nilCompare(aVal, bVal); ok {
			return c
		}
		c := compare(reflect.ValueOf(aVal.Elem().Type()), reflect.ValueOf(bVal.Elem().Type()))
		if c != 0 {
			return c
		}
		return compare(aVal.Elem(), bVal.Elem())
	default:
		// Certain types cannot appear as keys (maps, funcs, slices), but be explicit.
		panic("bad type in compare: " + aType.String())
	}
}

// nilCompare checks whether either value is nil. If not, the boolean is false.
// If either value is nil, the boolean is true and the integer is the comparison
// value. The comparison is defined to be 0 if both are nil, otherwise the one
// nil value compares low. Both arguments must represent a chan, func,
// interface, map, pointer, or slice.
func nilCompare(aVal, bVal reflect.Value) (int, bool) {
	if aVal.IsNil() {
		if bVal.IsNil() {
			return 0, true
		}
		return -1, true
	}
	if bVal.IsNil() {
		return 1, true
	}
	return 0, false
}

// floatCompare compares two floating-point values. NaNs compare low.
func floatCompare(a, b float64) int {
	switch {
	case isNaN(a):
		return -1 // No good answer if b is a NaN so don't bother checking.
	case isNaN(b):
		return 1
	case a < b:
		return -1
	case a > b:
		return 1
	}
	return 0
}

func isNaN(a float64) bool {
	return a != a
}
