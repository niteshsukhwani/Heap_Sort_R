

print("Program to sort an array using Heap Sort")

#arr = the array or vector which we have to sort
#n  = size of the vetor
#ch = choise whether we want ascending order or descending order

buildHeap <- function(arr, n ,ch) 
{
  for (i in n:1)
  {arr = maxHeapify(arr, i, n,ch)}
  return (arr)
}



maxHeapify <- function(arr, i, n, ch) 
{
  l=2*i
  r=2*i+1
  if(ch)
  {
    if(l<=n && arr[l] > arr[i])
    {lrg=l}
    else
    {lrg=i}
    if(r<=n && arr[r] > arr[lrg])
    {lrg=r}
    if(lrg!=i)
    {
      tmp=arr[lrg]
      arr[lrg]=arr[i]
      arr[i]=tmp
      arr = maxHeapify(arr,lrg,n,ch)
    }
  }
  else
  {
    if(l<=n && arr[l] < arr[i])
    {lrg=l}
    else
    {lrg=i}
    if(r<=n && arr[r] < arr[lrg])
    {lrg=r}
    if(lrg!=i)
    {
      tmp=arr[lrg]
      arr[lrg]=arr[i]
      arr[i]=tmp
      arr = maxHeapify(arr,lrg,n,ch)
    }
  }
  
  
  
  return (arr)
}



heapSort <- function(arr,n,ch) 
{
  arr = buildHeap(arr, n,ch)
  print(arr)
  for (i in n:2)
  {  tmp=arr[i]
  arr[i]=arr[1]
  arr[1]=tmp
  n=n-1
  arr = maxHeapify(arr, 1, n,ch)
  }
  return (arr)
}
