module ord
    implicit none

contains
    
    subroutine merg (v, l, m, r)
        
        integer, intent(inout) :: v(:) ! vetor
        integer, intent(in) :: l, m, r ! indice de inicio meio e fim
        
        integer :: i, j, k                     ! iteradores
        integer :: size_l, size_r, size_temp   ! tamanhos dos vetores auxiliares
        
        integer, allocatable :: left(:), right(:), temp(:) ! vetores auxiliares
        
        size_l = m - l + 1
        size_r = r - m
        size_temp = r - l + 1
        
        allocate (left(size_l))
        allocate (right(size_r))
        allocate (temp(size_temp))
        
        ! preenchendo os vetores l e r
        do i = 1, size_l
            left(i) = v(l+i-1)
        end do
        
        do j = 1, size_r
            right(j) = v(m+j)
        end do
        
        i = 1
        j = 1
        k = 1
        
        ! inserindo na ordem 
        do while (i <= size_l .and. j <= size_r)
        
            if (left(i) <= right(j)) then
            
                temp(k) = left(i)
                i = i + 1
                
            else
            
                temp(k) = right(j)
                j = j + 1
                
            end if
            
            k = k + 1
            
        end do
        
        ! inserindo os elementos restantes
        do while (i <= size_l) 
        
            temp(k) = left(i);
            i = i + 1
            k = k + 1
        
        end do
        
        do while (j <= size_r) 
        
            temp(k) = right(j);
            j = j + 1
            k = k + 1
        
        end do
        
        ! copiando os elementos de volta / reordenando os ponteiros
        v(l:r) = temp(1:)
        
        deallocate(left)
        deallocate(right)
        deallocate(temp)
    
    end subroutine merg
    
    
    recursive subroutine mergeSort(v, l, r)
        integer, intent(inout) :: v(:) ! vetor
        integer, intent(in) :: l, r    ! indice de inicio e fim
        
        if (l < r) then
            block
                integer :: m
                m = (l+r)/2
                
                call mergeSort(v, l, m);
                call mergeSort(v, m+1, r);
                
                call merg(v, l, m, r);
                
            end block
        end if
    
    end subroutine mergeSort
    
end module ord
