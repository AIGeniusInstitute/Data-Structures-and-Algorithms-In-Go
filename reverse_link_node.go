package Data_Structures_and_Algorithms_In_Go

// ReverseLinkNode 反转一个带有头结点（dummy head）的单向链表
func ReverseLinkNode(node *LNode) *LNode {
	// 如果链表为空或只有一个实际节点，则无需反转
	if node == nil || node.Next == nil {
		return node
	}

	// node: head->1->2->3->4->5->nil
	var pre *LNode
	// cur 当前节点: cur=1->2->3->4->5->nil
	var cur = node.Next

	// 遍历整个链表
	for cur != nil {
		// 保存 next 节点的下一个节点 next=2->3->4->5->nil，防止断链
		next := cur.Next
		// 将 cur 节点的 Next 指针指向 pre，实现反转 1->nil
		cur.Next = pre

		// pre 指针向后移动，指向刚刚反转的节点 pre=1->nil
		pre = cur
		// cur 指針向后移动，处理下一个节点 next=2->3->4->5->nil
		cur = next
	}

	// 循环结束后，pre 指向反转后链表的头节点： 将原始头结点的 Next 指向 pre，完成整个链表的反转
	node.Next = pre

	return node
}
