# ZQUERY
Open SQL executing/testing application on SAP AS ABAP

<details><summary><h2>Installation</h2></summary>
 Upload files and screens to your system and add GUI status manually by following below provided instructions( SAP doesnt directly allow download/upload interface for GUI-statuses,you will abapgit or SAPLINK to download and upload them ).
 ## Adding GUI-statuses
  Right click on program name and choose create -> Gui Status
  ![image](https://user-images.githubusercontent.com/43263062/173181509-a89a329a-91c3-472f-b6c8-1e03d5a8970c.png)
<details><summary><h3>Adding STATUS (Example)</h3></summary>
Create status
![image](https://user-images.githubusercontent.com/43263062/173181999-c036f6f3-d361-475e-bb1a-0a7428fb585d.png)

You will be presented with the status screen,
![image](https://user-images.githubusercontent.com/43263062/173181668-92a03c06-f48c-4f33-a95c-ef6e37663992.png)

add item to application toolbar,
Add Fuction code to first Input: NEW
![image](https://user-images.githubusercontent.com/43263062/173182403-c89afe22-3e41-4439-987a-1f1caa64978f.png)
select OK.
![image](https://user-images.githubusercontent.com/43263062/173181728-ad9d9119-d52e-44e7-af1e-c5a6121ef233.png)
Double click and select F5 or any function key of your choice and select OK.
![image](https://user-images.githubusercontent.com/43263062/173181737-e0648bca-c709-4b47-a4f7-db430090e31e.png)

fill relavant Info and save.
![image](https://user-images.githubusercontent.com/43263062/173181811-7a980291-cd9e-4d5f-bc55-8c7fa3105c97.png)
</details>
 
Below table contains list of Statuses/Items to be added:
|Status Name|	Short Text|	Status Type|
|-----------|-----------|------------|
|PF_9000|	Main Status|	Normal Screen|
|PF_9100|	Query Wizard|	Dialog Box|
|PF_9200|	Setting|	Dialog Box|
|STATUS9300|	Save Option|	Dialog Box|
<details><summary><h3>PF_9000</h3></summary>
Application toolbar items
|Function Code|Type       |Function Text|Icon Name|Icon Text|Info.Text|Field Name|
|-------------|-----------|-------------|---------|---------|---------|----------|
|NEW          |Static Text|NEW|ICON_ADD_ROW||New Tab||
|DEL_TAB      |Static Text|DEL_TAB|ICON_REMOVE_ROW||Delete Tab||
|             |separator Line||
|QWIZ         |Static Text|Query Wizard|ICON_WIZARD||Query Wizard||
|             |separator Line||
|CHECK        |Static Text|Execute|ICON_EXECUTE_OBJECT|Execute|||
|EXEC2PC      |Static Text|Execute to PC|ICON_WRITE_FILE|Execute to file|||
|EXEC2BG      |Static Text|Execute as Job|ICON_SYM_SPOOL_SERVER|Execute as a Background Job|||
|EXEC2BG      |Static Text|Execute as Job|ICON_SYM_SPOOL_SERVER|Execute as a Background Job|||
|             |separator Line||
|SETTING      |Static Text|Settings|ICON_SETTINGS|Settings|||
|EXP|Static Text|COL/EXP|ICON_VIEW_EXPAND_HORIZONTAL||||
||separator Line||
|MAX_ROW|Dynamic Text|Execute as Job|ICON_SYM_SPOOL_SERVER|Execute as a Background Job||W-MAX_ROW|

Create Separator line by right clicking on item
![image](https://user-images.githubusercontent.com/43263062/173182795-597a78f9-4598-4f1e-841b-3a4206db5590.png)
Add Function keys as shown in image
![image](https://user-images.githubusercontent.com/43263062/173182818-d6c33953-017f-4b9b-a82c-c861bb5b0419.png)
</details>
 <details><summary><h3>PF_9100</h3></summary>
|Function Code|Type|Function Text|Icon Name|Icon Text|Info.Text|Field Name|
|-----------|-----------|------------|--------|------|-----|----|
|OK_QWIZ|Static Text|Close|ICON_OKAY|||
|CLOSE_QWIZ|Static Text|Cancel|ICON_CANCEL|||
  </details>
<details><summary><h3>PF_9200</h3></summary>
 <p>
|Function Code|Type|Function Text|Icon Name|Icon Text|Info.Text|Field Name|
|-----------|-----------|------------|--------|------|-----|----|
|OK_SET|Static Text|Close|ICON_OKAY|blank|blank|blank|
 </p>
</details>
  </details>

 
