package Data_Structures_and_Algorithms_In_Go

import "fmt"

// ReverseLinkNode 反转一个带有头结点（dummy head）的单向链表
func ReverseLinkNode(node *LNode) {
	// 如果链表为空或只有一个实际节点，则无需反转
	if node == nil || node.Next == nil {
		return
	}

	// node: 头结点->1->2->3->4->5->nil

	// 定义三个指针：pre、cur、next
	// pre 指向反转后链表的前一个节点，初始为 nil
	var pre *LNode
	// cur 当前节点
	var cur *LNode
	// next 指向 cur 下一个节点
	var next *LNode

	// 从链表的第一个实际节点开始 next=1->2->3->4->5->nil
	next = node.Next
	fmt.Println("===> 执行：next = node.Next")

	var step int = 0

	fmt.Println("开始循环")
	// 遍历整个链表
	for next != nil {
		step++
		stepInfo := fmt.Sprintf("-------------step:%v---------------", step)
		fmt.Println(stepInfo)

		PrintNode("0 node:", node)
		PrintNode("0 pre:", pre)
		PrintNode("0 cur:", cur)
		PrintNode("0 next:", next)

		// 保存 next 节点的下一个节点 cur=2->3->4->5->nil，防止断链
		cur = next.Next
		fmt.Println("===> 执行：cur = next.Next")
		PrintNode("1 node:", node)
		PrintNode("1 pre:", pre)
		PrintNode("1 cur:", cur)
		PrintNode("1 next:", next)

		// 将 next 节点的 Next 指针指向 pre，实现反转 1->nil
		next.Next = pre
		fmt.Println("===> 执行：next.Next = pre")
		PrintNode("2 node:", node)
		PrintNode("2 pre:", pre)
		PrintNode("2 cur:", cur)
		PrintNode("2 next:", next)

		// pre 指针向后移动，指向刚刚反转的节点 pre=1
		pre = next
		fmt.Println("===> 执行：pre = next")
		PrintNode("3 node:", node)
		PrintNode("3 pre:", pre)
		PrintNode("3 cur:", cur)
		PrintNode("3 next:", next)

		// next 指針向后移动，处理下一个节点 next=2
		next = cur

		fmt.Println("===> 执行：next = cur")
		PrintNode("4 node:", node)
		PrintNode("4 pre:", pre)
		PrintNode("4 cur:", cur)
		PrintNode("4 next:", next)

		fmt.Println(stepInfo)

	}

	fmt.Println("结束循环")

	// 循环结束后，pre 指向反转后链表的头节点
	// 将原始头结点的 Next 指向 pre，完成整个链表的反转
	node.Next = pre
	fmt.Println("===> 执行：node.Next = pre")
	PrintNode("node:", node)
	PrintNode("pre:", pre)
	PrintNode("cur:", cur)
	PrintNode("next:", next)

}
