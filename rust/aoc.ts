/**
 * Definition for singly-linked list.
 **/
class ListNode {
  val: number;
  next: ListNode | null;
  constructor(val?: number, next?: ListNode | null) {
    this.val = val === undefined ? 0 : val;
    this.next = next === undefined ? null : next;
  }
}

function hasCycle(head: ListNode | null): boolean {
  let fast: ListNode | null | undefined = head;
  let slow: ListNode | null | undefined = head;
  while (true) {
    fast = fast?.next?.next;
    slow = slow?.next;
    if (!fast) return false;
    if (fast === slow) return true;
  }
}
