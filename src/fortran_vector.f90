module fortran_vector
   implicit none
   private
   public::vector_int,size
   type::vector_int
      integer,allocatable::x_(:)
      integer::num_
      integer::capacity_
   contains
      generic::append=>vector_append_val,vector_append_array
      procedure,pass::init  =>vector_init
      procedure,pass::pop   =>vector_pop
      procedure,pass::get   =>vector_get
      procedure,pass::delete=>vector_delete
      procedure,pass::remove=>vector_remove
      procedure,pass::write =>vector_write
      procedure,pass::clear =>vector_clear
      procedure,pass::sort  =>vector_sort
      procedure,pass::unique  =>vector_unique
      procedure,pass::cut  =>vector_cut
      procedure,pass,private::vector_set_capacity
      procedure,pass,private::vector_append_val
      procedure,pass,private::vector_append_array
   end type vector_int
   interface size
      module procedure vector_size
   end interface size

   interface optval
      module procedure optval_int
      module procedure optval_char
   end interface optval
contains
   integer function optval_int(i,default)result(res)
      integer,optional,intent(in)::i
      integer,intent(in)::default
      if(present(i))then
         res=i
      else
         res=default
      end if
   end function optval_int

   function optval_char(i,default)result(res)
      character(len=*),optional,intent(in)::i
      character(len=*),intent(in)::default
      character(len=:),allocatable::res
      if(present(i))then
         res=i
      else
         res=default
      end if
   end function optval_char

   function vector_size(this)result(n)
      !! size of vector
      type(vector_int),intent(in)::this
      integer::n
      n=this%num_
   end function vector_size


   subroutine vector_init(this,capacity_)
      !! init ,capacity_ is optional,set 100
      class(vector_int),intent(inout)::this
      integer,optional,intent(in)::capacity_
      this%num_= 0
      this%capacity_=optval(capacity_,100)
      allocate(this%x_(this%capacity_))
   end subroutine vector_init

   subroutine vector_set_capacity(this,arr)
      class(vector_int),intent(inout)::this
      integer,optional,intent(in)::arr
      integer::capacity
      integer,allocatable::tmp(:)
      capacity=this%capacity_+this%capacity_/3+1+optval(arr,1)
      allocate(tmp(capacity))
      tmp(1:this%capacity_)=this%x_
      deallocate(this%x_)
      call move_alloc(from=tmp,to=this%x_)
      this%capacity_=capacity
   end subroutine vector_set_capacity

   subroutine vector_append_val(this,a_)
      !! append   val
      class(vector_int),intent(inout)::this
      integer,intent(in)::a_
      if(this%num_+1>this%capacity_)then
         call this%vector_set_capacity()
      end if
      this%num_=this%num_+1
      this%x_(this%num_)=a_
   end subroutine vector_append_val

   subroutine vector_append_array(this,a_)
      !! append   array
      class(vector_int),intent(inout)::this
      integer,intent(in)::a_(:)
      integer::n
      n=size(a_)
      if(this%num_+n>this%capacity_)then
         call this%vector_set_capacity(n)
      end if
      this%x_(this%num_+1:this%num_+n)=a_
      this%num_=this%num_+n
   end subroutine vector_append_array

   integer function vector_pop(this)result(res)
      !! pop  last element
      class(vector_int),intent(inout)::this
      res=this%x_(this%num_)
      this%num_=this%num_-1
   end function vector_pop

   subroutine vector_delete(this,i)
      !! delete idx element
      class(vector_int),intent(inout)::this
      integer,intent(in)::i
      this%x_(i:this%num_-1)=this%x_(i+1:this%num_)
      this%num_=this%num_-1
   end subroutine vector_delete

   integer function vector_get(this,idx)result(res)
      !! get idx element
      class(vector_int),intent(inout)::this
      integer,intent(in)::idx
      res=this%x_(idx)
   end function vector_get

   subroutine vector_write(this,fmt)
      !! write vector,fmt is optional
      class(vector_int),intent(inout)::this
      character(len=*),optional,intent(in)::fmt
      write(*,fmt=optval(fmt,"(*(g0,1x))"))this%x_(1:this%num_)
   end subroutine vector_write

   subroutine vector_clear(this)
      !! clear vector
      class(vector_int),intent(inout)::this
      deallocate(this%x_)
      this%num_=0
      this%capacity_=0
   end subroutine vector_clear

   subroutine vector_remove(this,a)
      !! remove element a
      class(vector_int),intent(inout)::this
      integer,intent(in)::a
      integer::num,i
      num=0
      do i=1,this%num_
         if(this%x_(i)/=a)then
            num=num+1
            this%x_(num)=this%x_(i)
         end if
      end do
      this%num_=num
   end subroutine vector_remove

   subroutine vector_unique(this)
      !! remove duplicate elements, order keep
      class(vector_int),intent(inout)::this
      integer::first,val
      first=this%x_(1)
      call this%remove(this%x_(1))
      call this%append(first)
      do
         val=this%x_(1)
         if(val==first)exit
         call this%remove(val)
         call this%append(val)
      end do
   end subroutine vector_unique

   subroutine vector_cut(this)
      !! if don't append, cut array
      class(vector_int),intent(inout)::this
      this%x_=this%x_(1:this%num_)
      this%capacity_=this%num_
   end subroutine vector_cut

   subroutine vector_sort(this)
      !! sort
      class(vector_int),intent(inout)::this
      integer::num,i
      call qsortc(this%x_(1:this%num_))
   end subroutine vector_sort
  
   !! http://fcode.cn/code_prof-38-1.html 
   recursive subroutine qsortc(a)
      integer, intent(inout) :: a(:)
      integer :: iq
      if(size(a) > 1) then
         call partition(a, iq)
         call qsortc(a(:iq-1))
         call qsortc(a(iq:))
      endif
   end subroutine qsortc

   subroutine partition(a, marker)
      integer, intent(inout):: a(:)
      integer, intent(out) :: marker
      integer :: i, j
      integer :: x
      x = a(1)
      i= 0
      j= size(a) + 1
      do
         j = j-1
         do
            if (a(j) <= x) exit
            j = j-1
         end do
         i = i+1
         do
            if (a(i) >= x) exit
            i = i+1
         end do
         if (i < j) then
            a([i,j])=a([j,i])
         elseif (i == j) then
            marker = i+1
            return
         else
            marker = i
            return
         endif
      end do
   end subroutine partition
end module fortran_vector
