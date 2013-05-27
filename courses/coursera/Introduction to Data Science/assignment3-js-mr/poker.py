import json
import MapReduce

mr = MapReduce.MapReduce()

def compute_hands():
    faces = [face for face in '23456789TJQKA']
    suits = [suit for suit in 'SCDH']  
    all_cards = [face + suit for face in faces for suit in suits]
    jenc = json.JSONEncoder() 
    outfile = open('hands.json', 'w')
    for i1, c1 in enumerate(all_cards): 
        for i2, c2 in enumerate(all_cards[i1+1:]):
            for i3, c3 in enumerate(all_cards[i1+i2+2:]): 
                for i4, c4 in enumerate(all_cards[i1+i2+i3+3:]): 
                    for c5 in all_cards[i1+i2+i3+i4+4:]: 
                        hand = ('%s,%s,%s,%s,%s' % (c1, c2, c3, c4, c5))
                        outfile.write(jenc.encode(hand) + '\n')
    outfile.close()




def mapper(record):
    cards = record.split(',')   # 5 cards like 'QH' (for Q of hearts)  
    
    # Get counts of all faces and suits. 
    counts = ({ 
          '2':0, '3':0, '4':0, '5':0, '6':0, '7':0, '8':0, '9':0, 'T':0, 
          'J':0, 'Q':0, 'K':0, 'A':0, 
          'S':0, 'C':0, 'D':0, 'H':0 
       }) 
    for card in cards: 
        face = card[0] 
        suit = card[1] 
        counts[face] += 1 
        counts[suit] += 1
    
    is_flush = ( 
          (counts['S'] == 5) or 
          (counts['C'] == 5) or 
          (counts['D'] == 5) or 
          (counts['H'] == 5)) 
    
    is_straight = False
    straightrunfaces = 'A23456789TJQKA';   # note: ace ('A') appears twice
    for i in range(0, 10): 
        if (counts[straightrunfaces[i]] and 
              counts[straightrunfaces[i+1]] and 
              counts[straightrunfaces[i+2]] and 
              counts[straightrunfaces[i+3]] and 
              counts[straightrunfaces[i+4]]): 
            is_straight = True 
            break

    # new fake poker hand
    is_4cards_straight = False
    for i in range(0, 11): 
        if (counts[straightrunfaces[i]] and 
              counts[straightrunfaces[i + 1]] and 
              counts[straightrunfaces[i + 2]] and 
              counts[straightrunfaces[i + 3]]): 
            is_4cards_straight = True 
            break 
    
    is_quad, is_trip, is_pair, is_two_pair = False, False, False, False 
    faces = 'A23456789TJQK' 
    for i in range(0, len(faces)): 
        face_count = counts[faces[i]] 
        if face_count == 4: 
            is_quad = True 
        elif face_count == 3: 
            is_trip = True 
        elif face_count == 2: 
            if is_pair:   # saw another pair before? 
                is_two_pair = True 
            is_pair = True 
    
    # Emit output: a (stringized) count of '1' for the detected hand.
    if is_straight and is_flush: 
        mr.emit_intermediate('straightflush', '1') 
    elif is_quad: 
        mr.emit_intermediate('4ofakind', '1') 
    elif is_trip and is_pair: 
        mr.emit_intermediate('fullhouse', '1') 
    elif is_flush: 
        mr.emit_intermediate('flush', '1') 
    elif is_straight: 
        mr.emit_intermediate('straight', '1') 
    elif is_4cards_straight:
        mr.emit_intermediate('4cardstraight', '1')
    elif is_trip: 
        mr.emit_intermediate('3ofakind', '1') 
    elif is_two_pair: 
        mr.emit_intermediate('2pair', '1') 
    elif is_pair: 
        mr.emit_intermediate('pair', '1') 
    else: 
        mr.emit_intermediate('highcard', '1') 

def reducer(key, list_of_values):
    sum = 0; 
    for value in list_of_values: 
        count = int(value) # convert to int for summing 
        sum += count 

    output_str = '%s:%d' % (key, sum) 
    mr.emit(output_str)




def main():
    try:
        inputdata = open('hands.json')
    except IOError:
        compute_hands()
        inputdata = open('hands.json')
    mr.execute(inputdata, mapper, reducer)

if __name__ == '__main__':
    main()