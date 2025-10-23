package Data_Structures_and_Algorithms_In_Go

import (
	"fmt"
	"testing"
)

// 测试Slice的Stack
func TestSliceStatck(t *testing.T) {
	defer func() {
		if err := recover(); err != nil {
			fmt.Println(err)
		}
	}()
	stack := NewSliceStack()
	stack.Push(1)
	Equal(t, 1, stack.Top().(int))
	Equal(t, 1, stack.Size())
	NotNil(t, stack.Pop())
	Nil(t, stack.Pop())
	Equal(t, 0, stack.Size())
}

// 测试Linked的Stack
func TestLinkedStatck(t *testing.T) {
	defer func() {
		if err := recover(); err != nil {
			fmt.Println(err)
		}
	}()
	stack := NewLinkedStack()
	stack.Push(1)
	Equal(t, 1, stack.Top())
	Equal(t, 1, stack.Size())
	stack.Pop()
	Equal(t, 0, stack.Size())
}
