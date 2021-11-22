import time
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait, Select
from selenium.webdriver.support import expected_conditions as EC
from products import products

def init():
    global driver
    options = webdriver.ChromeOptions()
    options.add_argument("--disable-blink-features=AutomationControlled")
    driver = webdriver.Chrome(options=options)
    driver.get('https://me.sumup.com/en-en/items')

def tag(name, tag = 'input'):
    for name_input in driver.find_elements(By.TAG_NAME, tag):
        if name_input.get_attribute('name') == name:
            return name_input
    raise RuntimeError(f'Tag {tag} with name {name} not found.')

def submit():
    for sb in driver.find_elements(By.TAG_NAME, 'button'):
        if '>Save<' in sb.get_attribute('innerHTML'):
            break
    sb.click()

def new_item(name, category, description, vat, variants):
    #driver.get('https://me.sumup.com/en-en/items')
    #WebDriverWait(driver, 30).until(EC.presence_of_element_located((By.CLASS_NAME, 'css-1ccti6r')))
    while True:
        try:
            driver.find_element(By.CLASS_NAME, 'css-1ccti6r').click()
            break
        except: pass
    counter = 30
    while counter > 0:
        counter -= 1
        time.sleep(1)
        forms = [y for y in driver.find_elements(By.TAG_NAME, 'button') if 'ecepsgj0' in y.get_attribute('class')]
        if len(forms) > 1:
            break
    forms[0].click() # description
    if len(variants) > 1:
        forms[1].click() # variants
    if len(forms) > 2:
        forms[2].click() # category

    category = category.replace('/', '-')
    tag('name').send_keys(name)
    category_input = tag('category', tag = 'select')
    categories = [cat.text for cat in category_input.find_elements(By.TAG_NAME, 'option')]
    if category in categories:
        Select(category_input).select_by_visible_text(category)
    else: tag('newCategoryName').send_keys(category)
    if description:
        tag('description', tag = 'textarea').send_keys(description)
    if vat not in ('8', '10', '11.11', '15', '25'):
        vat = '8'
    Select(tag('taxRateId', tag = 'select')).select_by_visible_text(f'{vat}%')
    added = 2
    for num, var in enumerate(variants):
        if num >= added:
            driver.find_element(By.CLASS_NAME, 'ecepsgj0').click()
        if var[0]:
            tag(f'variants[{num}].name').send_keys(var[0])
        tag(f'variants[{num}].price').send_keys(var[1])
    time.sleep(3)
    submit()

def create_products():
    res = []
    for prod in products:
        name = prod['name']
        vat = prod['vatPercentage']
        if prod['category']:
            category = prod['category']['name']
        else: category = 'Default'
        description = prod['description']
        variants = []
        for var in prod['variants']:
            if var['price']:
                variants.append((var['name'], var['price']['amount']/100))
        res.append((name, category, description, vat, variants))
    return res

def add_products():
    global prods
    while len(prods) > 0:
        p = prods.pop()
        print(len(prods), p)
        try: new_item(*p)
        except Exception as err:
            prods.append(p)
            print("ERROR:", err)
            break
