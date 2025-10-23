package Data_Structures_and_Algorithms_In_Go

import (
	"fmt"
	"io"
	"reflect"
	"runtime"
	"strings"
	"testing"
	"time"
)

var errorPrefix = "\U0001F4A9 "

// -- Assertion handlers

func assert(t *testing.T, success bool, f func(), callDepth int) {
	if !success {
		_, file, line, _ := runtime.Caller(callDepth + 1)
		t.Errorf("%s:%d", file, line)
		f()
		t.FailNow()
	}
}

func equal(t *testing.T, expected, got interface{}, callDepth int, messages ...interface{}) {
	fn := func() {
		for _, desc := range Diff(expected, got) {
			t.Error(errorPrefix, desc)
		}
		if len(messages) > 0 {
			t.Error(errorPrefix, "-", fmt.Sprint(messages...))
		}
	}
	assert(t, isEqual(expected, got), fn, callDepth+1)
}

func notEqual(t *testing.T, expected, got interface{}, callDepth int, messages ...interface{}) {
	fn := func() {
		t.Errorf("%s Unexpected: %#v", errorPrefix, got)
		if len(messages) > 0 {
			t.Error(errorPrefix, "-", fmt.Sprint(messages...))
		}
	}
	assert(t, !isEqual(expected, got), fn, callDepth+1)
}

func contains(t *testing.T, expected, got string, callDepth int, messages ...interface{}) {
	fn := func() {
		t.Errorf("%s Expected to find: %#v", errorPrefix, expected)
		t.Errorf("%s in: %#v", errorPrefix, got)
		if len(messages) > 0 {
			t.Error(errorPrefix, "-", fmt.Sprint(messages...))
		}
	}
	assert(t, strings.Contains(got, expected), fn, callDepth+1)
}

func notContains(t *testing.T, unexpected, got string, callDepth int, messages ...interface{}) {
	fn := func() {
		t.Errorf("%s Expected not to find: %#v", errorPrefix, unexpected)
		t.Errorf("%s in: %#v", errorPrefix, got)
		if len(messages) > 0 {
			t.Error(errorPrefix, "-", fmt.Sprint(messages...))
		}
	}
	assert(t, !strings.Contains(got, unexpected), fn, callDepth+1)
}

func withinDuration(t *testing.T, duration time.Duration, goalTime, gotTime time.Time, callDepth int, messages ...interface{}) {
	fn := func() {
		t.Errorf("%s Expected %v to be within %v of %v", errorPrefix, gotTime, duration, goalTime)
		if len(messages) > 0 {
			t.Error(errorPrefix, "-", fmt.Sprint(messages...))
		}
	}
	actualDuration := goalTime.Sub(gotTime)
	if actualDuration < time.Duration(0) {
		actualDuration = -actualDuration
	}
	assert(t, actualDuration <= duration, fn, callDepth+1)
}

// -- Matching

func isEqual(expected, got interface{}) bool {
	if expected == nil {
		return isNil(got)
	} else {
		return reflect.DeepEqual(expected, got)
	}
}

func isNil(got interface{}) bool {
	if got == nil {
		return true
	}
	value := reflect.ValueOf(got)
	switch value.Kind() {
	case reflect.Chan, reflect.Func, reflect.Interface, reflect.Map, reflect.Ptr, reflect.Slice:
		return value.IsNil()
	default:
		return false
	}
}

// -- Public API

func Equal(t *testing.T, expected, got interface{}, messages ...interface{}) {
	equal(t, expected, got, 1, messages...)
}

func NotEqual(t *testing.T, expected, got interface{}, messages ...interface{}) {
	notEqual(t, expected, got, 1, messages...)
}

func True(t *testing.T, got interface{}, messages ...interface{}) {
	equal(t, true, got, 1, messages...)
}

func False(t *testing.T, got interface{}, messages ...interface{}) {
	equal(t, false, got, 1, messages...)
}

func Nil(t *testing.T, got interface{}, messages ...interface{}) {
	equal(t, nil, got, 1, messages...)
}

func NotNil(t *testing.T, got interface{}, messages ...interface{}) {
	notEqual(t, nil, got, 1, messages...)
}

func Contains(t *testing.T, expected, got string, messages ...interface{}) {
	contains(t, expected, got, 1, messages...)
}

func NotContains(t *testing.T, unexpected, got string, messages ...interface{}) {
	notContains(t, unexpected, got, 1, messages...)
}

func WithinDuration(t *testing.T, duration time.Duration, goalTime, gotTime time.Time, messages ...interface{}) {
	withinDuration(t, duration, goalTime, gotTime, 1, messages...)
}

// Uses recover() to check for a panic.
// Usually used as `defer assert.Panic(expected)`.
func Panic(t *testing.T, expected interface{}, messages ...interface{}) {
	got := recover()
	equal(t, expected, got, 1, messages...)
}

type sbuf []string

func (p *sbuf) Printf(format string, a ...interface{}) {
	s := fmt.Sprintf(format, a...)
	*p = append(*p, s)
}

// Diff returns a slice where each element describes
// a difference between a and b.
func Diff(a, b interface{}) (desc []string) {
	Pdiff((*sbuf)(&desc), a, b)
	return desc
}

// wprintfer calls Fprintf on w for each Printf call
// with a trailing newline.
type wprintfer struct{ w io.Writer }

func (p *wprintfer) Printf(format string, a ...interface{}) {
	fmt.Fprintf(p.w, format+"\n", a...)
}

// Fdiff writes to w a description of the differences between a and b.
func Fdiff(w io.Writer, a, b interface{}) {
	Pdiff(&wprintfer{w}, a, b)
}

type Printfer interface {
	Printf(format string, a ...interface{})
}

// Pdiff prints to p a description of the differences between a and b.
// It calls Printf once for each difference, with no trailing newline.
// The standard library log.Logger is a Printfer.
func Pdiff(p Printfer, a, b interface{}) {
	d := diffPrinter{
		w:        p,
		aVisited: make(map[visit]visit),
		bVisited: make(map[visit]visit),
	}
	d.diff(reflect.ValueOf(a), reflect.ValueOf(b))
}

type Logfer interface {
	Logf(format string, a ...interface{})
}

// logprintfer calls Fprintf on w for each Printf call
// with a trailing newline.
type logprintfer struct{ l Logfer }

func (p *logprintfer) Printf(format string, a ...interface{}) {
	p.l.Logf(format, a...)
}

// Ldiff prints to l a description of the differences between a and b.
// It calls Logf once for each difference, with no trailing newline.
// The standard library testing.T and testing.B are Logfers.
func Ldiff(l Logfer, a, b interface{}) {
	Pdiff(&logprintfer{l}, a, b)
}

type diffPrinter struct {
	w Printfer
	l string // label

	aVisited map[visit]visit
	bVisited map[visit]visit
}

func (w diffPrinter) printf(f string, a ...interface{}) {
	var l string
	if w.l != "" {
		l = w.l + ": "
	}
	w.w.Printf(l+f, a...)
}

func (w diffPrinter) diff(av, bv reflect.Value) {
	if !av.IsValid() && bv.IsValid() {
		w.printf("nil != %# v", formatter{v: bv, quote: true})
		return
	}
	if av.IsValid() && !bv.IsValid() {
		w.printf("%# v != nil", formatter{v: av, quote: true})
		return
	}
	if !av.IsValid() && !bv.IsValid() {
		return
	}

	at := av.Type()
	bt := bv.Type()
	if at != bt {
		w.printf("%v != %v", at, bt)
		return
	}

	if av.CanAddr() && bv.CanAddr() {
		avis := visit{av.UnsafeAddr(), at}
		bvis := visit{bv.UnsafeAddr(), bt}
		var cycle bool

		// Have we seen this value before?
		if vis, ok := w.aVisited[avis]; ok {
			cycle = true
			if vis != bvis {
				w.printf("%# v (previously visited) != %# v", formatter{v: av, quote: true}, formatter{v: bv, quote: true})
			}
		} else if _, ok := w.bVisited[bvis]; ok {
			cycle = true
			w.printf("%# v != %# v (previously visited)", formatter{v: av, quote: true}, formatter{v: bv, quote: true})
		}
		w.aVisited[avis] = bvis
		w.bVisited[bvis] = avis
		if cycle {
			return
		}
	}

	switch kind := at.Kind(); kind {
	case reflect.Bool:
		if a, b := av.Bool(), bv.Bool(); a != b {
			w.printf("%v != %v", a, b)
		}
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		if a, b := av.Int(), bv.Int(); a != b {
			w.printf("%d != %d", a, b)
		}
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		if a, b := av.Uint(), bv.Uint(); a != b {
			w.printf("%d != %d", a, b)
		}
	case reflect.Float32, reflect.Float64:
		if a, b := av.Float(), bv.Float(); a != b {
			w.printf("%v != %v", a, b)
		}
	case reflect.Complex64, reflect.Complex128:
		if a, b := av.Complex(), bv.Complex(); a != b {
			w.printf("%v != %v", a, b)
		}
	case reflect.Array:
		n := av.Len()
		for i := 0; i < n; i++ {
			w.relabel(fmt.Sprintf("[%d]", i)).diff(av.Index(i), bv.Index(i))
		}
	case reflect.Chan, reflect.Func, reflect.UnsafePointer:
		if a, b := av.Pointer(), bv.Pointer(); a != b {
			w.printf("%#x != %#x", a, b)
		}
	case reflect.Interface:
		w.diff(av.Elem(), bv.Elem())
	case reflect.Map:
		ak, both, bk := keyDiff(av.MapKeys(), bv.MapKeys())
		for _, k := range ak {
			w := w.relabel(fmt.Sprintf("[%#v]", k))
			w.printf("%q != (missing)", av.MapIndex(k))
		}
		for _, k := range both {
			w := w.relabel(fmt.Sprintf("[%#v]", k))
			w.diff(av.MapIndex(k), bv.MapIndex(k))
		}
		for _, k := range bk {
			w := w.relabel(fmt.Sprintf("[%#v]", k))
			w.printf("(missing) != %q", bv.MapIndex(k))
		}
	case reflect.Ptr:
		switch {
		case av.IsNil() && !bv.IsNil():
			w.printf("nil != %# v", formatter{v: bv, quote: true})
		case !av.IsNil() && bv.IsNil():
			w.printf("%# v != nil", formatter{v: av, quote: true})
		case !av.IsNil() && !bv.IsNil():
			w.diff(av.Elem(), bv.Elem())
		}
	case reflect.Slice:
		lenA := av.Len()
		lenB := bv.Len()
		if lenA != lenB {
			w.printf("%s[%d] != %s[%d]", av.Type(), lenA, bv.Type(), lenB)
			break
		}
		for i := 0; i < lenA; i++ {
			w.relabel(fmt.Sprintf("[%d]", i)).diff(av.Index(i), bv.Index(i))
		}
	case reflect.String:
		if a, b := av.String(), bv.String(); a != b {
			w.printf("%q != %q", a, b)
		}
	case reflect.Struct:
		for i := 0; i < av.NumField(); i++ {
			w.relabel(at.Field(i).Name).diff(av.Field(i), bv.Field(i))
		}
	default:
		panic("unknown reflect Kind: " + kind.String())
	}
}

func (d diffPrinter) relabel(name string) (d1 diffPrinter) {
	d1 = d
	if d.l != "" && name[0] != '[' {
		d1.l += "."
	}
	d1.l += name
	return d1
}

// keyEqual compares a and b for equality.
// Both a and b must be valid map keys.
func keyEqual(av, bv reflect.Value) bool {
	if !av.IsValid() && !bv.IsValid() {
		return true
	}
	if !av.IsValid() || !bv.IsValid() || av.Type() != bv.Type() {
		return false
	}
	switch kind := av.Kind(); kind {
	case reflect.Bool:
		a, b := av.Bool(), bv.Bool()
		return a == b
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		a, b := av.Int(), bv.Int()
		return a == b
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		a, b := av.Uint(), bv.Uint()
		return a == b
	case reflect.Float32, reflect.Float64:
		a, b := av.Float(), bv.Float()
		return a == b
	case reflect.Complex64, reflect.Complex128:
		a, b := av.Complex(), bv.Complex()
		return a == b
	case reflect.Array:
		for i := 0; i < av.Len(); i++ {
			if !keyEqual(av.Index(i), bv.Index(i)) {
				return false
			}
		}
		return true
	case reflect.Chan, reflect.UnsafePointer, reflect.Ptr:
		a, b := av.Pointer(), bv.Pointer()
		return a == b
	case reflect.Interface:
		return keyEqual(av.Elem(), bv.Elem())
	case reflect.String:
		a, b := av.String(), bv.String()
		return a == b
	case reflect.Struct:
		for i := 0; i < av.NumField(); i++ {
			if !keyEqual(av.Field(i), bv.Field(i)) {
				return false
			}
		}
		return true
	default:
		panic("invalid map key type " + av.Type().String())
	}
}

func keyDiff(a, b []reflect.Value) (ak, both, bk []reflect.Value) {
	for _, av := range a {
		inBoth := false
		for _, bv := range b {
			if keyEqual(av, bv) {
				inBoth = true
				both = append(both, av)
				break
			}
		}
		if !inBoth {
			ak = append(ak, av)
		}
	}
	for _, bv := range b {
		inBoth := false
		for _, av := range a {
			if keyEqual(av, bv) {
				inBoth = true
				break
			}
		}
		if !inBoth {
			bk = append(bk, bv)
		}
	}
	return
}
