/* Sizes */
`define WORD [15:0]
`define HALFWORD [7:0]
`define STACKSIZE [511:0] //double stack size (so no neeed to alter sp size)
`define OPCODE [15:12]
`define IMMEDIATE12 [11:0]
`define IMMEDIATE [11:0]
`define REGSIZE [15:0]
`define STATE [4:0]
`define BOTTOMFOURBITS [3:0]
`define MEMSIZE [131071:0]//double mem size (so no need to alter pc size)

/* 12-bit Opcodes */
`define GetOp 4'b0000
`define PopOp 4'b0001
`define PutOp 4'b0010

/* 16-bit Opcode Vals  */
`define CallOp 4'b0011
`define JumpFOp 4'b0100
`define JumpTOp 4'b0101
`define JumpOp 4'b0110
`define PushOp 4'b0111

/* Pre Opcode */
`define PreOp 4'b1110

/* No Arg ALU Opcode */
`define NoArgAluOp 4'b1111
`define AddOp 16
`define SubOp 17
`define OrOp 18
`define XorOp 19
`define LtOp 20
`define AndOp 21

/* Other NoArg Opcodes */
`define DupOp 4'b1000
`define LoadOp 4'b1001
`define ReturnOp 4'b1010
`define StoreOp 4'b1011
`define SystemOp 4'b1100
`define TestOp 4'b1101

`define Start	5'b11111
`define Nop	5'b11110

//-----------------

module processor(halt[1], halt[0], reset, clk);
output reg [1:0] halt;//a halt for each thread
input reset, clk;

//stage0
reg [14:0] pc [1:0]; //pc is one bit smaller, as the top bit is the thread number
reg `WORD ir;//encoded instruction
reg thread[3:0]; //which thread the instruction was loaded from (set in stage 0 and passed forward)

//stage1
reg `HALFWORD dest[2:1];//these two are the registers we may be using
reg `HALFWORD src[2:1];//calculated in stage 1 and used in stage 2
reg `HALFWORD sp [1:0];//a stack pointer for each thread

//stage2
reg `WORD stackregs `STACKSIZE;
reg [3:0] pre [1:0];// pre register for each thread
reg loaded [1:0]; // 1-bit register for each thread to track if pre has been loaded or not
reg `WORD pcwrite;//the data to put in the pc
reg pcwriteflag;//does stage 2 want to write to pc? (so we can ask stage 0 nicely)
reg `WORD data[3:2]; //the data from register (calculated in 2 and used in 3)
reg `WORD addr[3:2]; //either additional data or the memory address from register

//stage3
reg `HALFWORD writeDest[3:2];//the register we want to write to (calculated in stage 2 and used in stage 3)
reg `WORD writeData;//the data to put in the register
reg regwriteflag;//does stage 3 want to write to a register? (so we can ask stage 2 nicely)
reg torf [1:0]; // true or false registers for each thread (will only be set here)

//stage instructions
reg `STATE s [3:0];//instruction states for each stage (0 is unused)
reg `IMMEDIATE immediate [3:0];// immediate from instruction for each stage (0 is unused)

//other
reg [15:0] i; //for test feedback
reg [15:0] k; //test feedback
reg `WORD mainmem `MEMSIZE; 


always @(reset) begin

  halt [1] = 0;
  halt [0] = 0;

  //stage 0
  pc [1] = -1; //* Changed from 0 to -1
  pc [0] = -1;
  thread[0] = 0;//starts with thread 0

  //stage 1
  sp [1] = -1;//stack pointer is incremented first
  sp [0] = -1;
  src[1] = 0;
  dest[1] = 0;

  //stage 2
  loaded [1] = 0;
  loaded [0] = 0;
  pre [1] = 0;
  pre [0] = 0;

  //stage 3
  torf [1] = 0;
  torf [0] = 0;
  
  //initial stage states
  s [3] = `Nop;
  s [2] = `Nop;
  s [1] = `Nop;
  s [0] = `Nop;

  pcwriteflag = 0;
  regwriteflag = 0;

  $readmemh0(mainmem);//This is a Von Neumann, only one memory (BE VERY CAREFUL WHERE YOU WRITE TO)
end

//forward stage instructions
always @(posedge clk) 
begin
	s[3] <= s[2];
	s[2] <= s[1];
	immediate[3] <= immediate[2];
	immediate[2] <= immediate[1];
	thread[0] = ~thread[0];
	writeDest[3] <= writeDest[2];
	dest[2] <= dest[1];
	src[2] <= src[1];
	thread[3] <= thread[2];//it may have been easier to simply alternate each, but this allows us to easily shut down one thread if we wanted
	thread[2] <= thread[1];
	thread[1] <= thread[0];
	data[3] <= data[2]; 
	addr[3] <= addr[2];
end

//stage0
always @(posedge clk) 
begin
	if(halt[thread[0]]) begin
		//don't do anything in a halted thread
	end

	else begin
		
		if( pcwriteflag ) begin//does stage 2 want to write to pc?
			pc[thread[0]] = pcwrite;//put the new pc value in
			//pcwriteflag <= 0;
		end
		ir <= mainmem[{thread[0], pc[thread[0]]}]; //top bit in address denotes which thread we read from
		pc[thread[0]] <= pc[thread[0]] + 1;
	end
end

//stage1
always @(posedge clk) 
begin
	if(halt[thread[1]]) begin
		s[1] = `Nop;//load nops for halted thread
		immediate[1] = 0;
	end

	else begin
		/* 
		If OPCODE is 15, look in the lower 5 bits of the immediate field.
		Else, execute load normal instruction state found in the OPCODE field.
		*/
		if( ir `OPCODE == `NoArgAluOp )	begin
			s[1] = ir `STATE;
		end
		else begin
			s[1] = ir `OPCODE;
		end
		immediate[1] = ir `IMMEDIATE;
	end
	

	//stack pointer alterations and register decoding
	case (s[1])
    	`Start: begin 
		//this shouldn't happen, but if it does it's a nop
	end
   	`Nop: begin
      		//nothing
    	end
    	`AddOp: begin 
	//d=sp-1; s=sp; --sp; reg[d]+=reg[s];
        	dest[1] = {thread[1], sp[thread[1]] - 1'b1};//top bit in register number is the thread number (thus giving half the stack to each thread)
		src[1] = {thread[1], sp[thread[1]]};
		sp[thread[1]] = sp[thread[1]] - 1'b1;
		$display("1Add");
	end
	`SubOp: begin 
	//d=sp-1; s=sp; --sp; reg[d]-=reg[s];
                dest[1] = {thread[1], sp[thread[1]] - 1'b1};//top bit in register number is the thread number (thus giving half the stack to each thread)
		src[1] = {thread[1], sp[thread[1]]};
		sp[thread[1]] = sp[thread[1]] - 1'b1;
		$display("1Sub");
	end
	`OrOp: begin
	//d=sp-1; s=sp; --sp; reg[d]|=reg[s];
                dest[1] = {thread[1], sp[thread[1]] - 1'b1};//top bit in register number is the thread number (thus giving half the stack to each thread)
		src[1] = {thread[1], sp[thread[1]]};
		sp[thread[1]] = sp[thread[1]] - 1'b1;
		$display("1Or");
	end
	`XorOp: begin 
	//d=sp-1; s=sp; --sp; reg[d]^=reg[s];
                dest[1] = {thread[1], sp[thread[1]] - 1'b1};//top bit in register number is the thread number (thus giving half the stack to each thread)
		src[1] = {thread[1], sp[thread[1]]};
		sp[thread[1]] = sp[thread[1]] - 1'b1;
		$display("1Xor");
	end
	`LtOp: begin 
	//d=sp-1; s=sp; --sp; reg[d]=(reg[d] < reg[s]);
                dest[1] = {thread[1], sp[thread[1]] - 1'b1};//top bit in register number is the thread number (thus giving half the stack to each thread)
		src[1] = {thread[1], sp[thread[1]]};
		sp[thread[1]] = sp[thread[1]] - 1'b1;
		$display("1Lt");
	end
	`AndOp: begin 
	//d=sp-1; s=sp; --sp; reg[d]&=reg[s];
                dest[1] = {thread[1], sp[thread[1]] - 1'b1};//top bit in register number is the thread number (thus giving half the stack to each thread)
		src[1] = {thread[1], sp[thread[1]]};
		sp[thread[1]] = sp[thread[1]] - 1;
		$display("1And");
	end	
	
	`GetOp: begin
	//d=sp+1; s=sp-unsigned(immed12); ++sp; reg[d]=reg[s];
                dest[1] = {thread[1], sp[thread[1]] + 1'b1};//you get the picture
		src[1] = {thread[1], sp[thread[1]] - ir `IMMEDIATE};
		sp[thread[1]] = sp[thread[1]] + 1;
		$display("1Get");
	end
	
	`PopOp: begin
	//sp-=unsigned(immed12);
                sp[thread[1]] = sp[thread[1]] - ir `IMMEDIATE;
		$display("1Pop");
	end
	
	`PutOp: begin
	//d=sp-unsigned(immed12); s=sp; reg[d]=reg[s];
		dest[1] = {thread[1], sp[thread[1]] - ir `IMMEDIATE};
		src[1] = {thread[1], sp[thread[1]]};
		$display("1Put");
	end
	
	`CallOp: begin
	//d=sp+1; ++sp; reg[d]=pc+1; pc=prefix({(pc>>12), immed12});
		dest[1] = {thread[1], sp[thread[1]] + 1'b1};
		sp[thread[1]] = sp[thread[1]] + 1;
		$display("1Call");
	end	
	
	`JumpFOp: begin
	//if (!torf) pc=prefix({(pc>>12), immed12});
		//nothing till next stage
		$display("1JumpF");
	end
	
	`JumpTOp: begin
	//if (torf) pc=prefix({(pc>>12), immed12});
		//nothing till next stage
		$display("1JumpT");
	end
	
	`JumpOp: begin
	//pc=prefix({(pc>>12), immed12});
		//nothing till next stage
		$display("1Jump");
	end

	`PushOp: begin
	//d=sp+1; ++sp; reg[d]=prefix(sign_extend(immed12));
                dest[1] = {thread[1], sp[thread[1]] + 1'b1};
		sp[thread[1]] = sp[thread[1]] + 1;
		$display("1Push");
	end
	
	`DupOp: begin
	//d=sp+1; s=sp; ++sp; reg[d]=reg[s];
		dest[1] = {thread[1], sp[thread[1]] + 1'b1};
		src[1] = {thread[1], sp[thread[1]]};
		sp[thread[1]] = sp[thread[1]] + 1;
		$display("1Dup");
	end
	
	`LoadOp: begin
	//d=sp; reg[d]=mem[reg[d]];
                dest[1] = {thread[1], sp[thread[1]]};
		$display("1Load");
	end
	
	`ReturnOp: begin
	//s=sp; --sp; pc=reg[s];
		src[1] = {thread[1], sp[thread[1]]};
		sp[thread[1]] = sp[thread[1]] - 1;
		$display("1Return");
	end
	
	`StoreOp: begin
	//d=sp-1; s=sp; --sp; mem[reg[d]]=reg[s]; reg[d]=reg[s];
                dest[1] = {thread[1], sp[thread[1]] - 1'b1};//top bit in register number is the thread number (thus giving half the stack to each thread)
		src[1] = {thread[1], sp[thread[1]]};
		sp[thread[1]] = sp[thread[1]] - 1;
		$display("1Store");
	end
	
	`SystemOp: begin                 
		halt[thread[1]] <= 1;
		$display("1System");
	end
	
	`TestOp: begin
	//s=sp; --sp; torf = (reg[s] != 0);
		src[1] = {thread[1], sp[thread[1]]};
		sp[thread[1]] = sp[thread[1]] - 1;
		$display("1Test");
	end
	
	`PreOp: begin
	//pre=unsigned(immed16)>>12;
		//nothing till next stage
		$display("1Pre");
	end
	
    	default: begin halt[thread[1]] <= 1; end
	endcase

end

//stage2
always @(posedge clk) 
begin
	//register values are handled
    	case (s[2])
    	`Start: begin 
		//this shouldn't happen, but if it does it's a nop
		pcwriteflag = 0;
	end
   	`Nop: begin
      		//nothing
		pcwriteflag = 0;
    	end
    	`AddOp: begin 
	//d=sp-1; s=sp; --sp; reg[d]+=reg[s];
        	writeDest[2] = dest[2];
		data[2] = stackregs[dest[2]];
		addr[2] = stackregs[src[2]];
		pcwriteflag = 0;
		$display("2Add");
		
	end
	`SubOp: begin 
	//d=sp-1; s=sp; --sp; reg[d]-=reg[s];
                writeDest[2] = dest[2];
		data[2] = stackregs[dest[2]];
		addr[2] = stackregs[src[2]];
		pcwriteflag = 0;
		$display("2Sub");
	end
	`OrOp: begin
	//d=sp-1; s=sp; --sp; reg[d]|=reg[s];
                writeDest[2] = dest[2];
		data[2] = stackregs[dest[2]];
		addr[2] = stackregs[src[2]];
		pcwriteflag = 0;
		$display("2Or");
	end
	`XorOp: begin 
	//d=sp-1; s=sp; --sp; reg[d]^=reg[s];
                writeDest[2] = dest[2];
		data[2] = stackregs[dest[2]];
		addr[2] = stackregs[src[2]];
		pcwriteflag = 0;
		$display("2Xor");
	end
	`LtOp: begin 
	//d=sp-1; s=sp; --sp; reg[d]=(reg[d] < reg[s]);
                writeDest[2] = dest[2];
		data[2] = stackregs[dest[2]];
		addr[2] = stackregs[src[2]];
		pcwriteflag = 0;
		$display("2Lt");
	end
	`AndOp: begin 
	//d=sp-1; s=sp; --sp; reg[d]&=reg[s];
                writeDest[2] = dest[2];
		data[2] = stackregs[dest[2]];
		addr[2] = stackregs[src[2]];
		pcwriteflag = 0;
		$display("2And");
	end	
	
	`GetOp: begin
	//d=sp+1; s=sp-unsigned(immed12); ++sp; reg[d]=reg[s];
                stackregs[dest[2]] = stackregs[src[2]];//taken care of
		pcwriteflag = 0;
		$display("2Get");
	end
	
	`PopOp: begin
	//sp-=unsigned(immed12);
                //nothing
		pcwriteflag = 0;
		$display("2Pop");
	end
	
	`PutOp: begin
	//d=sp-unsigned(immed12); s=sp; reg[d]=reg[s];
		stackregs[dest[2]] = stackregs[src[2]];//taken care of
		pcwriteflag = 0;
		$display("2Put");
	end
	
	`CallOp: begin
	//d=sp+1; ++sp; reg[d]=pc+1; pc=prefix({(pc>>12), immed12});
		stackregs[dest[2]] = pc[thread[2]] + 1;//the pc has already been incrimented by now (?)
		if( loaded[thread[2]] ) begin
			pcwrite = {pre[thread[2]], immediate[2]};//if using pre just tack pre onto the front of the immediate
		end
		else begin
			pcwrite = {pc[thread[2]][15:12], immediate[2]}; // if pre is not used, take top four bits of pc and immediate
		end
		loaded[thread[2]] = 0;
		pcwriteflag = 1;
		$display("2Call");
	end	
	
	`JumpFOp: begin
	//if (!torf) pc=prefix({(pc>>12), immed12});
		if(!torf[thread[2]]) begin
			if( loaded[thread[2]] ) begin
				pcwrite = {pre[thread[2]], immediate[2]};//if using pre just tack pre onto the front of the immediate
			end
			else begin
				pcwrite = {pc[thread[2]][15:12], immediate[2]}; // if pre is not used, take top four bits of pc and immediate
			end
			loaded[thread[2]] = 0;
			pcwriteflag = 1;
			$display("2JumpF");
		end
		else begin 
			pcwriteflag = 0;
		end
	end
	
	`JumpTOp: begin
	//if (torf) pc=prefix({(pc>>12), immed12});
		if(torf[thread[2]]) begin
			if( loaded[thread[2]] ) begin
				pcwrite = {pre[thread[2]], immediate[2]};//if using pre just tack pre onto the front of the immediate
			end
			else begin
				pcwrite = {pc[thread[2]][15:12], immediate[2]}; // if pre is not used, take top four bits of pc and immediate
			end
			loaded[thread[2]] = 0;
			pcwriteflag = 1;
			$display("2JumpT");
		end
		else begin 
			pcwriteflag = 0;
		end
	end
	
	`JumpOp: begin
	//pc=prefix({(pc>>12), immed12});
		if( loaded[thread[2]] ) begin
			pcwrite = {pre[thread[2]], immediate[2]};//if using pre just tack pre onto the front of the immediate
		end
		else begin
			pcwrite = {pc[thread[2]][15:12], immediate[2]}; // if pre is not used, take top four bits of pc and immediate
		end
		loaded[thread[2]] = 0;
		pcwriteflag = 1;
		$display("2Jump");
	end

	`PushOp: begin
	//d=sp+1; ++sp; reg[d]=prefix(sign_extend(immed12));
		if( loaded[thread[2]] ) begin
                	stackregs[dest[2]] = {pre[thread[2]], immediate[2]};//taken care of
		end
		else begin
			stackregs[dest[2]] = {4'b0000, immediate[2]};
		end
		pcwriteflag = 0;
		$display("2Push");
	end
	
	`DupOp: begin
	//d=sp+1; s=sp; ++sp; reg[d]=reg[s];
		stackregs[dest[2]] = stackregs[src[2]];//taken care of
		pcwriteflag = 0;
		$display("2Dup");
	end
	
	`LoadOp: begin
	//d=sp; reg[d]=mem[reg[d]];
                writeDest[2] = dest[2];
		addr[2] = stackregs[dest[2]];
		pcwriteflag = 0;
		$display("2Load");
	end
	
	`ReturnOp: begin
	//s=sp; --sp; pc=reg[s];

		pcwrite = stackregs[src[2]];
		pcwriteflag = 1;
		$display("2Return");
	end
	
	`StoreOp: begin
	//d=sp-1; s=sp; --sp; mem[reg[d]]=reg[s]; reg[d]=reg[s];
                stackregs[dest[2]] = stackregs[src[2]];
		addr[2] = stackregs[dest[2]];
		data[2] = stackregs[src[2]];
		pcwriteflag = 0;
		$display("2Store");
	end
	
	`SystemOp: begin                 
		//the thread should be halted by now
		pcwriteflag = 0;
		$display("2System");
	end
	
	`TestOp: begin
	//s=sp; --sp; torf = (reg[s] != 0);
		//leave it up to stage 3
		pcwriteflag = 0;
		$display("2Test");
	end
	
	`PreOp: begin
	//pre=unsigned(immed16)>>12;
		pre[thread[2]] = (immediate[2][3:0]);//pre is the bottom four bits of the immediate
		loaded[thread[2]] = 1;
		pcwriteflag = 0;
		$display("2Pre");
	end
	
    	default: begin halt[thread[2]] <= 1; end
	
	endcase

	//now to handle writes from stage 3
	if(regwriteflag)
	begin
		stackregs[writeDest[3]] = writeData;
	end
	//regwriteflag <= 0;
	/* Print the first 5 registers in each thread of every clock cycle */
	for( i = 0; i < 5; i = i + 1) begin
                 $display("reg[i]: %d i: %d", stackregs[i], i);
                end
	for (k = 256; k < 261; k = k+1) begin
		$display("reg[k]: %d k: %d", stackregs[k], k);
	end
end

//stage3
always @(posedge clk) 
begin
	//alu and memory operations
    	case (s[3])
    	`Start: begin 
		//this shouldn't happen, but if it does it's a nop
		regwriteflag = 0;
	end
   	`Nop: begin
      		//nothing
		regwriteflag = 0;
    	end
    	`AddOp: begin 
	//d=sp-1; s=sp; --sp; reg[d]+=reg[s];
        	writeData = data[3] + addr[3];
		regwriteflag = 1;
		$display("3Add");
	end
	`SubOp: begin 
	//d=sp-1; s=sp; --sp; reg[d]-=reg[s];
                writeData = data[3] - addr[3];
		regwriteflag = 1;
		$display("3Sub");
	end
	`OrOp: begin
	//d=sp-1; s=sp; --sp; reg[d]|=reg[s];
                writeData = data[3] | addr[3];
		regwriteflag = 1;
		$display("3Or");
	end
	`XorOp: begin 
	//d=sp-1; s=sp; --sp; reg[d]^=reg[s];
                writeData = data[3] ^ addr[3];
		regwriteflag = 1;
		$display("3Xor");
	end
	`LtOp: begin 
	//d=sp-1; s=sp; --sp; reg[d]=(reg[d] < reg[s]);
                writeData = data[3] < addr[3];
		regwriteflag = 1;
		$display("3Lt");
	end
	`AndOp: begin 
	//d=sp-1; s=sp; --sp; reg[d]&=reg[s];
                writeData = data[3] & addr[3];
		regwriteflag = 1;
		$display("3And");
	end	
	
	`GetOp: begin
	//d=sp+1; s=sp-unsigned(immed12); ++sp; reg[d]=reg[s];
                //taken care of
		regwriteflag = 0;
		$display("3Get");
	end
	
	`PopOp: begin
	//sp-=unsigned(immed12);
                //nothing
		regwriteflag = 0;
		$display("3Pop");
	end
	
	`PutOp: begin
	//d=sp-unsigned(immed12); s=sp; reg[d]=reg[s];
		//taken care of
		regwriteflag = 0;
		$display("3Put");
	end
	
	`CallOp: begin
	//d=sp+1; ++sp; reg[d]=pc+1; pc=prefix({(pc>>12), immed12});
		//taken care of
		regwriteflag = 0;
		$display("3Call");
	end	
	
	`JumpFOp: begin
	//if (!torf) pc=prefix({(pc>>12), immed12});
		//taken care of
		regwriteflag = 0;
		$display("3JumpF");
	end
	
	`JumpTOp: begin
	//if (torf) pc=prefix({(pc>>12), immed12});
		//taken care of
		regwriteflag = 0;
		$display("3JumpT");
	end
	
	`JumpOp: begin
	//pc=prefix({(pc>>12), immed12});
		//taken care of
		regwriteflag = 0;
		$display("3Jump");
	end

	`PushOp: begin
	//d=sp+1; ++sp; reg[d]=prefix(sign_extend(immed12));
                //taken care of
		regwriteflag = 0;
		$display("3Push");
	end
	
	`DupOp: begin
	//d=sp+1; s=sp; ++sp; reg[d]=reg[s];
		//taken care of
		regwriteflag = 0;
		$display("3Dup");
	end
	
	`LoadOp: begin
	//d=sp; reg[d]=mem[reg[d]];
                writeData = mainmem[{thread[3], addr[3]}];
		regwriteflag = 0;
		$display("3Load");
	end
	
	`ReturnOp: begin
	//s=sp; --sp; pc=reg[s];
		//taken care of
		regwriteflag = 0;
		$display("3Return");
	end
	
	`StoreOp: begin
	//d=sp-1; s=sp; --sp; mem[reg[d]]=reg[s]; reg[d]=reg[s];
                mainmem[{thread[3], addr[3]}] = data[3];
		regwriteflag = 0;
		$display("Store");
	end
	
	`SystemOp: begin                 
		//the thread should be halted by now
		regwriteflag = 0;
		$display("3System");
	end
	
	`TestOp: begin
	//s=sp; --sp; torf = (reg[s] != 0);
		torf[thread[2]] = (stackregs[src[2]] != 0);
		regwriteflag = 0;
		$display("3test");
	end
	
	`PreOp: begin
	//pre=unsigned(immed16)>>12;
		//taken care of
		regwriteflag = 0;
		$display("3Pre");
	end
	
    	default: begin halt[thread[2]] <= 1; end
	
	endcase

end

endmodule

module testbench;
	reg [15:0] i; //added for extra clock cycles
	reg reset = 0;
	reg clk = 0;
	wire [1:0] halted = 0;
	processor PE(halted[1], halted[0], reset, clk);
	initial begin
 		$dumpfile;
  		$dumpvars(0, PE);
  		#10 reset = 1;
  		#10 reset = 0;
 		while (!halted[1] | !halted[0]) begin //keep going until both threads halt
 	   		#10 clk = 1;
    			#10 clk = 0;
  		end
		for( i = 0; i < 6; i = i + 1) begin //extra clock cycles
                #10 clk = 1;
		#10 clk = 0;
                end
  		$finish;
	end
endmodule
