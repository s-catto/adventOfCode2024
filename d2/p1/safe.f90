module safe
    implicit none

contains
    
    ! retorna true caso o report seja safe
    logical function isSafe (r) result (safe)
        
        integer, intent(in) :: r(:)     ! report
        
        integer :: i                    ! iterador
        logical :: cresc                ! crescente (1) ou decrescente (0)
        integer :: dif                  ! diferenca
        
        safe = .true.
        
        ! se so ha um level, eh safe
        if (b(2) >= 0) then
            
            dif = b(1) - b(2)
            
            ! se a diferenca eh maior q 3
            if (abs(dif) > 3) then
                safe = .false.
            
            ! se a diferenca é negativa, cresc
            else if (dif < 0) then
                cresc = .true.
            ! se eh positiva, decresc 
            else if (dif > 0) then
                cresc = .false.
            ! se eh igual, unsafe
            else
                safe = .false.
            end if 
                        
            i = 3            
            do while (i <= size(b) .and. b(i) >= 0 .and. safe)
                
                dif = b(i-1) - b(i)
                
                ! se a diferenca eh maior que 3
                if (abs(dif) > 3) then
                    safe = .false.
                ! se eh igual
                else if (dif == 0) then
                    safe = .false.
                ! se cresce quando a sequencia eh decresc
                else if (dif < 0 .and. (.not. cresc)) then
                    safe = .false.
                !se decresc quando a sequencia eh cresc
                else if (dif > 0 .and. cresc) then
                    safe = .false.
                end if  
                
                i = i + 1
            end do
        
        end if
        
    end function isSafe
    
end module safe
