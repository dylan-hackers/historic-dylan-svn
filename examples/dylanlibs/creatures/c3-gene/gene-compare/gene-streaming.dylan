Module:    gene-compare
Synopsis:  A program to compare Creature gene files.
Author:    Chris Double
Copyright: (C) 1999, Chris Double. All rights reserved.
License:   See License.txt

define class <genome> (<object>)
  sealed constant slot genome-genes, required-init-keyword: genes:;
  sealed constant slot genome-filename = #f, init-keyword: filename:;
end class <genome>;

define sealed domain make(singleton(<genome>));
define sealed domain initialize(<genome>);

define open generic stream-gene( gene :: <gene>, genome :: <genome>, stream :: <stream>, #key note ) => ();

define function get-organ-number( gene :: <gene>, genome :: <genome>) => (result :: <integer>)
    block(return)
      let number :: <integer> = -1;
      for( current-gene in genome.genome-genes )
        when(instance?(current-gene, <organ-gene>))
          number := number + 1;
        end;
        when(current-gene == gene)
          return(number);
        end;
      end for;
      -1;
    end;
end function get-organ-number;

define function get-lobe-number( gene :: <gene>, genome :: <genome>) => (result :: <integer>)
    block(return)
      let number :: <integer> = -1;
      for( current-gene in genome.genome-genes )
        when(instance?(current-gene, <brain-lobe-gene>))
          number := number + 1;
        end;
        when(current-gene == gene)
          return(number);
        end;
      end for;
      -1;
    end;
end function get-lobe-number;

define function get-tract-number( gene :: <gene>, genome :: <genome>) => (result :: <integer>)
    block(return)
      let number :: <integer> = -1;
      for( current-gene in genome.genome-genes )
        when(instance?(current-gene, <brain-tract-gene>))
          number := number + 1;
        end;
        when(current-gene == gene)
          return(number);
        end;
      end for;
      -1;
    end;
end function get-tract-number;


define method stream-gene( gene :: <gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  format(stream, integer-to-string(gene.gene-index, size: 4, fill: ' '));
  format(stream, " ");
  when(note)
    format(stream, "%s ", note)
  end;
  format(stream, integer-to-string(gene.gene-sequence-number, size: 3, fill: ' '));
  format(stream, " ");
  format(stream, integer-to-string(gene.gene-duplicate-number, size: 3, fill: ' '));
  format(stream, " ");
  format(stream, 
    select(gene.gene-switch-on-stage)
      #"embryo" => "Emb";
      #"child" => "Chi";
      #"adolescent" => "Ado";
      #"youth" => "You";
      #"adult" => "Adu";
      #"old" => "Old";
      #"senile" => "Sen";
      otherwise => integer-to-string(gene.gene-switch-on-stage, size: 3, fill: ' ');
    end); 
  format(stream, " ");
  format(stream,
    select(gene.gene-switch-on-gender)
      #"both" => "B";
      #"male" => "M";
      #"female" => "F";
      otherwise => integer-to-string(gene.gene-switch-on-gender, size: 3, fill: ' ');
    end);
  format(stream, " ");
  format(stream, if(gene.gene-allow-mutations?) "Mut" else "   " end);
  format(stream, if(gene.gene-allow-duplicates?) "Dup" else "   " end);
  format(stream, if(gene.gene-allow-deletions?) "Cut" else "   " end);
  format(stream, if(gene.gene-is-dormant?) "Dor" else "   " end);
  format(stream, " ");
  format(stream, if(gene.gene-other-flags > 0) 
                   integer-to-string(gene.gene-other-flags, size: 3, fill: ' ')
                 else
                   "   " 
                 end);
  format(stream, " ");
  format(stream, integer-to-string(gene.gene-mutation-percentage, size: 3, fill: ' '));
  format(stream, " ");    
  format(stream, integer-to-string(gene.gene-variant, size: 3, fill: ' '));
  format(stream, " ");    
end method stream-gene;

define function stream-unknown-data( data, stream )
  let count :: <integer> = 0;
  let total-counts :: <integer> = 0;
  let ascii :: <string> = "|";
  format(stream, "                                            0000: ");
  for( value :: <integer> in data )
    format(stream, " %s", integer-to-string(value, base: 16, size: 2));
    ascii := concatenate(ascii,
                         if(
                           (value >= as(<integer>, 'A') &
                             value <= as(<integer>, 'Z')) |
                           (value >= as(<integer>, 'a') &
                             value <= as(<integer>, 'z')) |
                           (value >= as(<integer>, '0') &
                             value <= as(<integer>, '9')))
                               list(as(<character>, value));
                         else
                           ".";
                         end);
    count := count + 1;
    if(count == 8)
      total-counts := total-counts + 1;
      format(stream, "  %s|", ascii);
      ascii := "|";
      format(stream, "\r\n                                            %s%s", 
        integer-to-string(total-counts * 8, base: 16, size: 4), ": ");
      count := 0;
    end if;
  end for;
end function;

define method stream-gene( gene :: <brain-lobe-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  next-method();

  format(stream, "Lobe #=%d (%s) at X=%d Y=%d is %d neurons wide and %d neurons high.\r\n",
         get-lobe-number(gene, genome),
         gene.gene-brain-lobe-name,
         gene.gene-brain-lobe-x-position,
         gene.gene-brain-lobe-y-position,
         gene.gene-brain-lobe-width,
         gene.gene-brain-lobe-height);
 
  stream-unknown-data( gene.gene-brain-lobe-unknown, stream );
end method stream-gene;

define method stream-gene( gene :: <brain-tract-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  next-method();

  format(stream, "Tract #=%d, Unknown value=%d\r\n",
         get-tract-number(gene, genome),
         gene.gene-brain-tract-unknown1);
  format(stream, "                                            "
                 "From lobe %s cell %d to %d, with %d dendrites\r\n"
                 "                                            "
                 "to lobe %s cell %d to %d, with %d dendrites.\r\n",
         gene.gene-brain-tract-source-lobe,
         gene.gene-brain-tract-source-lower,
         gene.gene-brain-tract-source-upper,
         gene.gene-brain-tract-source-amount,
         gene.gene-brain-tract-destination-lobe,
         gene.gene-brain-tract-destination-lower,
         gene.gene-brain-tract-destination-upper,
         gene.gene-brain-tract-destination-amount);
  stream-unknown-data( gene.gene-brain-tract-unknown, stream );

end method stream-gene;

define method stream-gene( gene :: <receptor-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  next-method();

  format(stream, "Organ# = %d, %s, %s, %s, chem=%s, thresh=%d, nom=%d, gain=%d, features=%s%s (%d) ",
    get-organ-number(gene, genome),
    gene.gene-receptor-organ-description,
    gene.gene-receptor-tissue-description,
    gene.gene-receptor-locus-description,
    convert-to-chemical-description(gene.gene-genome-version, 
      gene.gene-receptor-chemical),
    gene.gene-receptor-threshold,
    gene.gene-receptor-nominal,
    gene.gene-receptor-gain,
    if(gene.gene-receptor-is-inverted?) "Inverted " else "" end,
    if(gene.gene-receptor-is-digital?) "Digital " else "Analogue " end,
    gene.gene-receptor-other-flags);
end method stream-gene;

define method stream-gene( gene :: <emitter-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  next-method();

  format(stream, "Organ# = %d, %s, %s, %s, chem=%s, thresh=%d, samp=%d, gain=%d, features=%s%s%s (%d) ",
    get-organ-number(gene, genome),
    gene.gene-emitter-organ-description,
    gene.gene-emitter-tissue-description,
    gene.gene-emitter-locus-description,
    convert-to-chemical-description(gene.gene-genome-version, 
      gene.gene-emitter-chemical),
    gene.gene-emitter-threshold,
    gene.gene-emitter-sample-rate,
    gene.gene-emitter-gain,
    if(gene.gene-emitter-is-inverted?) "Inverted " else "" end,
    if(gene.gene-emitter-is-digital?) "Digital " else "Analogue " end,
    if(gene.gene-emitter-clear-source?) "/Clear source " else "" end,
    gene.gene-emitter-other-flags);
end method stream-gene;

define method stream-gene( gene :: <neuro-emitter-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  local method lobe-getter( gene, lobe-number )
    let value :: <integer> = select(lobe-number) 
                  1 => gene.gene-neuro-emitter-lobe1;
                  2 => gene.gene-neuro-emitter-lobe2;
                  3 => gene.gene-neuro-emitter-lobe3;
                end;
    if(zero?(value) | value == 255)
      value
    else
      value - 1;
    end;
  end method;
    
  next-method();
  let v = gene.gene-genome-version;

  format(stream, "[%s] [%s] + [%s] [%s] + [%s] [%s] => %d*%s + %d*%s + %d*%s + %d*%s; Rate = %d. ",
    get-lobe-description(v, lobe-getter(gene, 1)),
    get-lobe-cell-description(v, lobe-getter(gene, 1), gene.gene-neuro-emitter-lobe1-cell),
    get-lobe-description(v, lobe-getter(gene, 2)),
    get-lobe-cell-description(v, lobe-getter(gene, 2), gene.gene-neuro-emitter-lobe2-cell),
    get-lobe-description(v, lobe-getter(gene, 3)),
    get-lobe-cell-description(v, lobe-getter(gene, 3), gene.gene-neuro-emitter-lobe3-cell),
    gene.gene-neuro-emitter-chemical1-amount,
    convert-to-chemical-description(v, gene.gene-neuro-emitter-chemical1),
    gene.gene-neuro-emitter-chemical2-amount,
    convert-to-chemical-description(v, gene.gene-neuro-emitter-chemical2),
    gene.gene-neuro-emitter-chemical3-amount,
    convert-to-chemical-description(v, gene.gene-neuro-emitter-chemical3),
    gene.gene-neuro-emitter-chemical4-amount,
    convert-to-chemical-description(v, gene.gene-neuro-emitter-chemical4),
    gene.gene-neuro-emitter-sample-rate);

end method stream-gene;

define method stream-gene( gene :: <reaction-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  next-method();

  format(stream, "Organ# = %d, %d*%s + %d*%s => %d*%s + %d*%s; half-life = %d ",
    get-organ-number(gene, genome),
    gene.gene-reaction-lhs1-amount,
    convert-to-chemical-description(gene.gene-genome-version, 
      gene.gene-reaction-lhs1),
    gene.gene-reaction-lhs2-amount,
    convert-to-chemical-description(gene.gene-genome-version, 
      gene.gene-reaction-lhs2),
    gene.gene-reaction-rhs1-amount,
    convert-to-chemical-description(gene.gene-genome-version, 
      gene.gene-reaction-rhs1),
    gene.gene-reaction-rhs2-amount,
    convert-to-chemical-description(gene.gene-genome-version, 
      gene.gene-reaction-rhs2),
    gene.gene-reaction-rate);
end method stream-gene;

define method stream-gene( gene :: <half-lives-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  next-method();

  let v = gene.gene-genome-version;
  for(chemical from 0 below gene.gene-half-lives.size)
    format(stream, "[%s] = %d ", convert-to-chemical-description(v, chemical), 
      gene.gene-half-lives[chemical]);
    when(~zero?(chemical) & zero?(modulo(chemical, 8)))
      format(stream, "\r\n                                            ");
    end when;
  end for;
      
end method stream-gene;


define method stream-gene( gene :: <initial-concentration-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  next-method();
  format(stream, "Initial concentration of %s is %d. ",
         convert-to-chemical-description(gene.gene-genome-version, gene.gene-initial-concentration-chemical),
         gene.gene-initial-concentration-amount);
         
end method stream-gene;

define method stream-gene( gene :: <stimulus-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  let v = gene.gene-genome-version;
  local method stimulus-chemical-translator (value :: <integer>)
     if(v == #"creatures2")
       value
     else
       case
         (value >= 0 & value <= 107) => value + 148;
         (value >= 108 & value <= 254) => value - 107;
         255 => 0;
         otherwise => value;
       end case;
     end;
   end method;

   local method chemical-amount( value :: <integer> )
     if(v == #"creatures2")
       value
     else
       value - 124;
     end;
   end method;

  next-method();

  format(stream, "%s (%d) causes sig=%d GS neu=%d int=%d, %s,%s,%s,%d => %d*%s + %d*%s + %d*%s + %d*%s ",
         convert-to-stimulus-description(v, gene.gene-stimulus-type),
         gene.gene-stimulus-type,
         gene.gene-stimulus-significance,
         gene.gene-stimulus-sensory-neuron,
         gene.gene-stimulus-intensity,
         if(gene.gene-stimulus-modulate?) "Modulate" else "" end,
         if(gene.gene-stimulus-sensed-asleep?) "Sensed even when asleep" else "" end,
         if(gene.gene-stimulus-add-offset?) "Added offset to neuron" else "" end,
         gene.gene-stimulus-other-feature,
         chemical-amount(gene.gene-stimulus-chemical1-amount),
         convert-to-chemical-description(v, stimulus-chemical-translator(gene.gene-stimulus-chemical1)),
         chemical-amount(gene.gene-stimulus-chemical2-amount),
         convert-to-chemical-description(v, stimulus-chemical-translator(gene.gene-stimulus-chemical2)),
         chemical-amount(gene.gene-stimulus-chemical3-amount),
         convert-to-chemical-description(v, stimulus-chemical-translator(gene.gene-stimulus-chemical3)),
         chemical-amount(gene.gene-stimulus-chemical4-amount),         
         convert-to-chemical-description(v, stimulus-chemical-translator(gene.gene-stimulus-chemical4)));
end method stream-gene;

define method stream-gene( gene :: <genus-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  next-method();

  format(stream, "It is genus %s. ", convert-to-description(<genus>, gene.gene-genus-species));
  when(gene.gene-genome-version == #"creatures2")
    format(stream, "Mum = %s, Dad = %s. ",
           gene.gene-genus-mum,
           gene.gene-genus-dad);
  end;

  when(gene.gene-genome-version == #"creatures3")
    format(stream, "\r\n");
    stream-unknown-data( gene.gene-genus-data, stream );
  end;
         
end method stream-gene;

define method stream-gene( gene :: <appearance-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  next-method();

  format(stream, "%s is of type %c, species %s. ",
         convert-to-description(<body-part>, gene.gene-appearance-body-part),
         as(<character>, 65 + gene.gene-appearance-breed),
         convert-to-description(<genus>, gene.gene-appearance-species));
         
         
end method stream-gene;

define method stream-gene( gene :: <pose-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  next-method();

  format(stream, "Pose %d (%s) = ", gene.gene-pose, convert-to-pose-description(gene.gene-pose));
  for(value in gene.gene-pose-data)
    format(stream, "%c ", as(<character>, value));
  end;         
end method stream-gene;

define method stream-gene( gene :: <gait-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  next-method();

  format(stream, "Sequence for %s = ",
         convert-to-description(<gait>, gene.gene-gait));
  for(value in gene.gene-gait-data)
    format(stream, "%d ", value);
  end;
end method stream-gene;

define constant $c3-drive-descriptions =  map(curry(get-emitter-locus-description, #"creatures3", 1, 5),
      range(from: 0, to: 19));

define function get-drive-description( number :: <integer> )
    element($c3-drive-descriptions,
      number,
      default: format-to-string("Unknown drive number: %d", number))
end function;

define method stream-gene( gene :: <instinct-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  let v = gene.gene-genome-version;
  local method lobe-getter( gene, lobe-number )
    let value :: <integer> = select(lobe-number) 
                  1 => gene.gene-instinct-lobe1;
                  2 => gene.gene-instinct-lobe2;
                  3 => gene.gene-instinct-lobe3;
                end;
    if(zero?(value) | value == 255)
      value
    else
      value - 1;
    end;
  end method;

  next-method();


  format(stream, "[%s] [%s] + [%s] [%s] + [%s] [%s] and [%s] => %s; unknown = %d. ",
    get-lobe-description(v, gene.gene-instinct-lobe1),
    get-lobe-cell-description(v, gene.gene-instinct-lobe1, gene.gene-instinct-lobe1-cell),
    get-lobe-description(v, gene.gene-instinct-lobe2),
    get-lobe-cell-description(v, gene.gene-instinct-lobe2, gene.gene-instinct-lobe2-cell),
    get-lobe-description(v, gene.gene-instinct-lobe3),
    get-lobe-cell-description(v, gene.gene-instinct-lobe3, gene.gene-instinct-lobe3-cell),
    get-drive-description(gene.gene-instinct-chemical),
    get-lobe-cell-description(v, 10, gene.gene-instinct-decision),
    gene.gene-instinct-chemical-amount);
end method stream-gene;

define method stream-gene( gene :: <pigment-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  next-method();

  format(stream, "Intensity of %s is = %s. ",
         convert-to-description(<pigmentation>, gene.gene-pigment-color),
         gene.gene-pigment-intensity);         
end method stream-gene;

define method stream-gene( gene :: <pigment-bleed-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  next-method();

  format(stream, "Rotation = %d, Swap = %d. ",
         gene.gene-pigment-bleed-rotation,
         gene.gene-pigment-bleed-swap);         
end method stream-gene;

define method stream-gene( gene :: <facial-expression-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  next-method();

  local method get-description( value :: <integer> )
    select(value) 
      0 => "Normal";
      1 => "Happy";
      2 => "Sad";
      3 => "Angry";
      4 => "Surprise";
      5 => "Sleepy";
      6 => "Very Sleepy";
      7 => "Very Happy";
      8 => "Mischevious";
      9 => "Scared";
      10 => "Ill";
      11 => "Hungry";
      otherwise => format-to-string("Expression %d", value);
    end;
  end method;

  format(stream, "%s, Weight %d => %d*%s + %d*%s + %d*%s + %d*%s ",
         get-description(gene.gene-expression-number),
         gene.gene-expression-weight,
         gene.gene-expression-drive1-amount - 124,
         get-drive-description(gene.gene-expression-drive1),
         gene.gene-expression-drive2-amount - 124,
         get-drive-description(gene.gene-expression-drive2),
         gene.gene-expression-drive3-amount - 124,
         get-drive-description(gene.gene-expression-drive3),
         gene.gene-expression-drive4-amount - 124,
         get-drive-description(gene.gene-expression-drive4));         
end method stream-gene;

define method stream-gene( gene :: <organ-gene>, genome :: <genome>, stream :: <stream>, #key note ) => ()
  next-method();

  format(stream, "Organ# = %d, Clockrate = %d, Life force repair rate = %d, "
                 "Life force start value = %d, Biotick start = %d, "
                 "ATP damage coefficient = %d ",
         get-organ-number(gene, genome),
         gene.gene-organ-clock-rate,
         gene.gene-organ-life-force-repair-rate,
         gene.gene-organ-life-force-start,
         gene.gene-organ-biotick-start,
         gene.gene-organ-atp-damage-coefficient);
end method stream-gene;


