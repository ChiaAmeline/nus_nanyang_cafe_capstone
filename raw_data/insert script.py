import csv
import io

# Your transaction CSV snippet
csv_test_data = """Serial Number,Date,Shop Name,Dish Code,Dish Name,Specification,Dish Category,Open Date Time,Checkout Date Time,Order Date Time,Selling Price,Transaction Quantity,Auxiliary Quantity,Transaction Amount,Discount Amount,Net Amount,Discount,Department,Dish Revenue Account,Bill Type,Channel,Flavour,Preparation Method,Shift,Area,Table Number,Order Taker,Cashier,Remarks,Bill Number,Notes
1,2024-01-31,南洋咖啡(卡爾店),30022,海南雞飯,份,單點,2024-01-31 19:54:32,2024-01-31 20:31:50,2024-01-31 19:54:42,58.00,1.00,0.00,58.00,0.00,58.00,10,后厨,商品销售收入,堂食,POS国际版,,,,大廳,A9,01|01,01|01,,2024013119543213717793310061,
2,2024-01-31,南洋咖啡(卡爾店),70028,南洋鴛鴦,凍,飲品,2024-01-31 19:54:32,2024-01-31 20:31:50,2024-01-31 19:54:42,18.00,1.00,0.00,18.00,0.00,18.00,10,水吧,商品销售收入,堂食,POS国际版,,,,大廳,A9,01|01,01|01,,2024013119543213717793310061,
3,2024-01-31,南洋咖啡(卡爾店),20011,三巴蘇東,份,南洋煮炒,2024-01-31 19:13:56,2024-01-31 19:54:16,2024-01-31 19:14:46,108.00,1.00,0.00,108.00,12.49,95.51,10,后厨,商品销售收入,堂食,POS国际版,,,,大廳,A6,01|01,01|01,,2024013119135613717793310060,
"""

# Your translation mapping CSV snippet
translation_csv = """海南雞飯,Hainanese Chicken Rice
南洋鴛鴦,Nanyang Coffee & Tea (Yuan Yang)
三巴蘇東,Sambal Sotong
白飯,Plain Rice
沙爹豬肉,Satay Pork
酸柑汁,Calamansi Juice
老虎啤酒,Tiger Beer
黑胡椒蝦,Black Pepper Prawns
咖哩雞配飯,Curry Chicken with Rice
椰漿飯,Nasi Lemak
打包盒,Takeaway Box
沙爹雞肉,Satay Chicken
咖椰多士,Kaya Toast
肉骨茶,Bak Kut Teh
肥牛叻沙,Fatty Beef Laksa
油菜一列,Boiled Vegetables
新加坡檸檬茶,Singapore Lemon Tea
馬來鹵面,Malaysian Braised Noodles
娘惹炸雞翅-3只,Nyonya Fried Chicken Wings (3 pcs)
淡奶咖啡,Condensed Milk Coffee (White Coffee)
南洋叻沙,Nanyang Laksa
米暹,Mee Siam
雞油飯,Plain Chicken Rice
咖哩雞配法棍,Curry Chicken with Baguette
自家製薏米水,Homemade Barley Drink
酸柑梳打,Calamansi Soda
三色奶茶,Three‑Coloured Milk Tea
雞飯叻沙套餐,Chicken Rice Laksa Set
辣椒蟹包配法棍,Chili Crab Bun with Baguette
打包袋,Takeaway Bag
椰汁雞肉米粉,Coconut Chicken Rice Vermicelli Soup
淡奶紅茶,Condensed Milk Black Tea
師傅撈面,Chef Dried Noodles
半熟蛋(2個）,Half‑Boiled Eggs (2 pcs)
雞飯肉骨茶套餐,Chicken Rice Bak Kut Teh Set
小份肉骨茶,Small Bak Kut Teh
參峇魚蛋,Sambal Fish Balls
咖椰醬,Kaya Spread
玫瑰奶露,Bandung Drink
蝦麵湯,Prawn Noodle Soup
花生醬咖椰多,Peanut Butter Kaya Toast
炒蝦面,Fried Hokkien Prawn Noodles
班蘭蛋糕-片,Pandan Cake (Slice)
煉奶咖啡,Condensed Milk Coffee
砂糖鮮油多,Sugar Butter Toast
法式燒蛋多,French Toast
黑咖啡,Black Coffee
服务费(A2),Service Fee (A2)
服务费(A9),Service Fee (A9)
服务费(A5),Service Fee (A5)
服务费(A20),Service Fee (A20)
服务费(A6),Service Fee (A6)
肉脞麵,Minced Pork Noodles
服务费(A10),Service Fee (A10)
美祿,Milo
服务费(A19),Service Fee (A19)
服务费(A8),Service Fee (A8)
服务费(A4),Service Fee (A4)
服务费(28),Service Fee (28)
服务费(25),Service Fee (25)
服务费(A15),Service Fee (A15)
服务费(A18),Service Fee (A18)
服务费(A16),Service Fee (A16)
服务费(A1),Service Fee (A1)
花生醬鮮油多士,Peanut Butter Toast
服务费(A7),Service Fee (A7)
服务费(A11),Service Fee (A11)
服务费(26),Service Fee (26)
服务费(A3),Service Fee (A3)
服务费(A17),Service Fee (A11)
蝦醬雞翅-3只,Shrimp Paste Chicken Wings (3 pcs)
烏打-3條,Otak‑Otak (3 pcs)
可口可樂,Coca‑Cola
大蝦叻沙,Jumbo Prawn Laksa
煉奶紅茶,Condensed Milk Black Tea
青檸梳打,Lime Soda
100PLUS,100PLUS
紅茶,Black Tea
番茄湯通粉,Tomato Macaroni Soup
三色咖啡,Three-Coloured Coffee
美祿恐龍,Milo Dinosaur
咖啡排骨,Coffee Pork Ribs
白糖咖啡,Sweetened Coffee
雪碧,Sprite
服务费(29),Service Fee (29)
檸檬水,Lemon Water
蒜炒時菜,Garlic Stir‑Fried Vegetables
仁當牛柳,Rendang Beef
海南雞半只,Half Hainanese Chicken
炒粿條,Fried Kway Teow
參峇秋葵,Sambal Okra
辣椒蝦配饅頭,Chili Prawns with Mantou
馬來風光,Malay‑Style Kangkung
100PLUS ZERO,100PLUS ZERO
白糖紅茶,Sweetened Black Tea
星州米粉,Singapore‑Style Fried Vermicelli
七彩撈起,Prosperity Toss Salad
馬來炒麵,Malay-Style Fried Noodles
蠔油菜（大）,Oyster Sauce Vegetables (Large)
蔘峇炒飯,Sambal Fried Rice
牛油黃金雞,Golden Butter Chicken
$30元配送費,$30 Delivery Fee
椰漿飯一碗, Plain Lemak Rice
法包,Baguette
咖央多士(打卡收藏半價),Kaya Toast (Half Price Social Media)
可口可樂+檸檬,Coke + Lemon
媽蜜雞,Marmite Chicken
海南雞一只,Whole Hainanese Chicken
服务费(35),Service Fee (35)
新馬福建炒麵,Hokkien Mee
釀油條,Youtiao
叻沙一人火鍋,Laksa Hot Pot (for one)
服务费(A14),Service Fee (A14)
烏打-6條,Otak‑Otak (6 pcs)
麥片蝦,Cereal Prawns
無糖可口可樂,Coke Zero
三水薑蓉雞,Sanshui Ginger Chicken
服务费(A13),Service Fee (A13)
參峇醬,Sambal Sauce
玉泉梳打水,Yujun Soda Water
咖椰多士+半熟蛋,Kaya Toast + Half‑Boiled Eggs
鹹蛋奶油蝦,Salted Egg Prawns
參巴烤魚,Sambal Grilled Fish
螃蟹米粉湯,Crab Bee Hoon Soup
咖哩魚頭,Curry Fish Head
訂餐收費$100,Ordering Fee $100
滑蛋河粉,Silky Egg Hor Fun
斑斕糯米糕,Kueh Salat
訂餐收費$1000,Ordering Fee $1000
南洋鴛鴦(熱),Hot Nanyang Yuan Yang
蠔煎,Oyster Omelette
烏打包,Takeaway Otak
訂餐收費$10,Ordering Fee $10
無糖可口可樂+檸檬,Coke Zero + Lemon
炒菜頭粿,Stir‑Fried Carrot Cake
斑蘭糯米糕,Kueh Salat
班蘭蛋糕-全個,Pandan Cake (Whole)
蝦醬雞扒,Shrimp Paste Chicken Chop
配送費,Delivery Fee
蒟蒻麵,Konjac Noodles
訂餐收費$1,Ordering Fee $1
100PLUS（24罐）,100PLUS (24 Cans)
服务费(A12),Service Fee (A12)
服务费(40),Service Fee (40)
服务费(46),Service Fee (46)
服务费(41),Service Fee (41)
服务费(50),Service Fee (50)
服务费(48),Service Fee (48)
服务费(49),Service Fee (49)
服务费(47),Service Fee (47)
服务费(42),Service Fee (42)
服务费(33),Service Fee (33)
服务费(27),Service Fee (27)
薄餅,Roti / Flatbread
椰奶奶茶,Coconut Milk Tea
三巴辣椒醬,Sambal Chili Sauce
美祿咖啡,Milo Coffee
咖哩角3個,Curry Puffs (3 pcs)
淡奶咖啡(熱),Hot Condensed Milk Coffee
淡奶紅茶(熱),Hot Condensed Milk Black Tea
白糖咖啡(凍),Iced Coffee with Sugar
服务费(38),Service Fee (38)
服务费(39),Service Fee (39)
自家製薏米水(凍),Iced Homemade Barley Drink
參峇蝦,Sambal Prawns
服务费(測試台),Service Fee (Test Table)
淡奶紅茶(凍),Iced Milk Tea
打包费,Takeaway Fee
餐盒费,Meal Box Fee
梳打水,Soda Water
服务费(30),Service Fee (30)
美祿(熱),Hot Milo
利賓納賓治,Ribena with Soda
淡奶咖啡(凍),Iced Condensed Milk Coffee
服务费(A18{~1}),Service Fee (A18~1)
椰奶咖啡,Coconut Milk Coffee
五香肉卷,Five‑Spice Meat Roll
南洋咖啡(卡爾店),Nanyang Coffee (Caravel Branch)
南洋咖啡（百老匯店）,Nanyang Coffee (Broadway Branch)
份,Portion
凍,Iced
杯,Cup
列,Strip/Row
熱,Hot
热,Hot
罐,Can
Pcs,Pieces
大,Large
小,Small
箱,Box
條,Piece (Strip/Bar)
單點,A la carte Order
飲品,Drinks
南洋煮炒,Nanyang Stir-fry
主食類,Main Course
小吃,Snacks
打包用品,Takeaway Packaging
房台服务费,Table Service Fee
其他,Others
專屬套餐飲品,Exclusive Set Meal Beverage
餐盒费,Meal Box Fee
應援專區,Support Zone
"""

# Load the translation mapping into a dictionary
translation_dict = {}
for line in translation_csv.strip().splitlines():
    key, val = line.split(',', 1)
    translation_dict[key.strip()] = val.strip()

def translate(value):
    # Translate if in dict, else return original
    return translation_dict.get(value, value)

def oracle_quote(value):
    if value is None or value.strip() == "":
        return "NULL"
    value = value.replace("'", "''")
    return f"'{value}'"

# Read from in-memory CSV (test)
csv_test_file = io.StringIO(csv_test_data)
reader = csv.DictReader(csv_test_file)

table_name = "tb_customer_transactions"
custom_columns = [
    'serial_number', 'date', 'shop_name', 'dish_code', 'dish_name',
    'specification', 'dish_category', 'open_date_time', 'checkout_date_time',
    'order_date_time', 'selling_price', 'transaction_quantity',
    'auxiliary_quantity', 'transaction_amount', 'discount_amount',
    'net_amount', 'discount', 'department', 'dish_revenue_account',
    'bill_type', 'channel', 'flavour', 'preparation_method', 'shift',
    'area', 'table_number', 'order_taker', 'cashier', 'remarks',
    'bill_number', 'notes'
]

input_csv = r'C:\Users\65916\Downloads\nanyang cafe data\original-transactions-2024.csv'
output_txt = r'C:\Users\65916\Downloads\nanyang cafe data\insert_statements.txt'
translated_csv = r'C:\Users\65916\Downloads\nanyang cafe data\translated-transactions-2024.csv'

# Process
with open(input_csv, newline='', encoding='utf-8') as csvfile, \
     open(output_txt, 'w', encoding='utf-8') as outfile, \
     open(translated_csv, 'w', newline='', encoding='utf-8') as translated_file:
    
    reader = csv.DictReader(csvfile)
    fieldnames = reader.fieldnames  # same as original CSV
    writer = csv.DictWriter(translated_file, fieldnames=fieldnames)
    writer.writeheader()  # write CSV headers

    for row in reader:
        # Translate selected columns
        for col in ['Dish Name', 'Shop Name', 'Specification', 'Dish Category']:
            if col in row:
                row[col] = translate(row[col])

        # Write translated row to new CSV
        writer.writerow(row)

        # Prepare INSERT statement from custom_columns
        columns = ', '.join(custom_columns)
        values = ', '.join(oracle_quote(row.get(col, '')) for col in custom_columns)
        sql = f"INSERT INTO {table_name} ({columns}) VALUES ({values});\n"

        # Write to SQL output file
        outfile.write(sql)

print(f"SQL insert statements written to:\n  {output_txt}")
print(f"Translated CSV written to:\n  {translated_csv}")