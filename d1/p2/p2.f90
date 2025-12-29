program p2
    use ord
    ! importante pra dizer q as vars vão ser todas declaradas explicitamente
    implicit none 
    
    integer :: tam                          ! tamanho da entrada
    integer, allocatable :: g1(:), g2(:)    ! vetores das duas listas
    
    integer :: input, stat                  ! variaveis controle p/ abrir os arq
    character(len=512) :: msg
    
    integer :: i, j                         ! iteradores
    integer :: freq, cont                   ! controles
    integer :: simi                         ! similaridade (resposta)
    
    print *, 'Insira o tamanho da lista.'
    read (*,*) tam
    
    ! alocacao dos vetores
    allocate (g1(tam))
    allocate (g2(tam))
    
    ! abrindo a lista
    open(newunit=input, file="../input", status="old", action="read", iostat=stat, iomsg=msg)
    if (stat /= 0) then
        print *, trim(msg)
        error stop
    end if
    
    ! lendo input
    do i = 1, tam
        read(input, *) g1(i), g2(i)
    end do
    
    close(input)
    
    ! ordenando os vetores
    call mergeSort(g1, 1, tam)
    call mergeSort(g2, 1, tam)
    
    ! calcula a similaridade
    i = 1
    j = 1
    
    simi = 0 ! resposta
    freq = 0 ! frequencia desse numero em g2
    cont = 1 ! contador desse numero em g1
    
    ! alinhando as duas listas no primeiro numero igual
    ! movimenta o iterador caso o número de g1 seja menor
    do while (g1(i) < g2(j) .and. i <= tam)
	    i = i + 1
	end do
	
	! || g2 seja menor
	do while (g1(i) > g2(j) .and. j <= tam)
	    j = j + 1
	end do
	
	! enquanto o numero de g2 corresponder ao g1, aumenta a frequencia 
	do while (g1(i) == g2(j) .and. j <= tam)
    	freq = freq + 1
    	j = j + 1
    end do
    
    i = i + 1
    do while (i <= tam .and. j <= tam)
    
        ! se esse número é igual ao aterior, mesma frequencia
        if (g1(i) == g1(i-1)) then
            cont = cont + 1
        
        ! c.c., update simi + 
        !       realinha listas + calcula a frequencia
        else
        	
        	! update
        	simi = simi + freq * g1(i-1) * cont
        	
        	freq = 0
        	cont = 1
            
            ! alinha
            do while (g1(i) > g2(j) .and. j <= tam)
                j = j + 1
            end do
            
            ! calc freq 
            do while (g1(i) == g2(j) .and. j <= tam)
            	freq = freq + 1
            	j = j + 1
            end do
        
        end if
        
        i = i + 1
        
    end do
    
    ! update
    simi = simi + freq * g1(i-1) * cont
    
    print *, simi
    
    ! desalocacao dos vetores
    deallocate (g1)
    deallocate (g2)
    
end program
