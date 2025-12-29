module safe
    implicit none

contains
    
    logical function isSafe (b) result (safe)
        
        integer, intent(in) :: b(:)
        
        integer :: i
        logical :: cresc
        integer :: dif
        
        safe = .true.
        
        if (b(2) >= 0) then
            
            dif = b(1) - b(2)
            
            if (abs(dif) > 3) then
                safe = .false.
            else if (dif < 0) then
                cresc = .true.
            else if (dif > 0) then
                cresc = .false.
            else
                safe = .false.
            end if 
                        
            i = 3            
            do while (i <= size(b) .and. b(i) >= 0 .and. safe)
                
                dif = b(i-1) - b(i)
                
                if (abs(dif) > 3) then
                    safe = .false.
                else if (dif == 0) then
                    safe = .false.
                else if (dif < 0 .and. (.not. cresc)) then
                    safe = .false.
                else if (dif > 0 .and. cresc) then
                    safe = .false.
                end if  
                
                i = i + 1
            end do
        
        end if
        
    end function isSafe
    
end module safe
