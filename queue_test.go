package Data_Structures_and_Algorithms_In_Go

import (
	"fmt"
	"testing"
)

func TestSliceQueue(t *testing.T) {
	defer func() {
		if err := recover(); err != nil {
			fmt.Println(err)
		}
	}()
	fmt.Println("Slice构建队列结构")
	queue := NewSliceQueue()
	queue.EnQueue(1)
	queue.EnQueue(2)
	Equal(t, 1, queue.GetFront())
	Equal(t, 2, queue.GetBack())
	Equal(t, 2, queue.Size())
}
func TestLinkedQueue(t *testing.T) {
	defer func() {
		if err := recover(); err != nil {
			fmt.Println(err)
		}
	}()
	fmt.Println("Slice构建队列结构")
	queue := NewLinkedQueue()
	queue.EnQueue(1)
	queue.EnQueue(2)
	Equal(t, 1, queue.GetFront())
	Equal(t, 2, queue.GetBack())
	Equal(t, 2, queue.Size())
}
