package Data_Structures_and_Algorithms_In_Go

import "testing"

func TestReverseLinkNode(t *testing.T) {
	head := &LNode{}

	CreateNode(head, 6)

	PrintNode("原始值:", head)

	ReverseLinkNode(head)

	PrintNode("逆序后:", head)
}
